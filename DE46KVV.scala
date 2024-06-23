object DE46KVV {
  def main(args: Array[String]): Unit = {}

/* Задание 4.6.
  Генерация и анализ треугольника Паскаля
  Цель этого задания - реализовать генератор треугольника Паскаля и провести анализ его свойств.

  Часть 1: Генератор треугольника Паскаля
  1. Создайте функцию на Scala, которая генерирует треугольник Паскаля до заданного уровня.
    Треугольник Паскаля представляется как список списков, где каждый список содержит числа для
    соответствующего уровня треугольника.
  2. При реализации используйте рекурсию или итерацию, чтобы генерировать треугольник.
 */

  val linesNum = 9 // задаём высоту треугольника (количество уровней)
  val basicLine = List(1) // Начальное число, являющееся вершиной треугольника Паскаля

  /* Основной метод, используемый здесь для вычисления треугольника, заключается в генерации каждой новой строки
  как суммы двух "сдвинутых" версий предыдущей строки, где сдвиг выполняется путем добавления нуля в конец строки.
  */

  // Функция сдвигает заданный список вправо, подставляя слева 0
  def shiftRight(row: List[Int]): List[Int] = 0 :: row

  // Сдвигает заданный список влево, подставляя справа 0, путём присоединения списка, содержащего одно значение - 0
  def shiftLeft(row: List[Int]): List[Int] = row ::: List(0)

  // функция для суммирования "сдвинутых" строк
  def addList(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1 zip l2) map { case (x, y) => x + y }
  }

  // функция вычисления треугольника Паскаля с использованием рекурсии и ленивых вычислений
  def pascalTriangleList(row: List[Int]): LazyList[List[Int]] = LazyList.cons(
    row,
    pascalTriangleList(addList(shiftLeft(row), shiftRight(row)))
  )

  val triangleL = pascalTriangleList(basicLine) // запись функции вычисления треугольника Паскаля в перменную
  // Запись в переменную треугольника паскаля в виде списка списков (задаётся количество уровней с помощью linesNum)
  val triangleList = triangleL.take(linesNum).toList
  println(s"Треугольник паскаля в виде списка списков (количество уровней: $linesNum): \n $triangleList") // вывод на экран треугольника Паскаля


  /*Часть 2: Анализ треугольника Паскаля
    3. Напишите функцию, которая принимает сгенерированный треугольник Паскаля и выводит его содержимое
      в удобном формате.
  */

  // Функция для вывода треугольника паскаля в удобном формате
  def printPascalTriangle(triangle: LazyList[List[Int]], numRows: Int) {
    val rows = triangle.take(numRows).map(_.mkString(" "))
    // Выравнивание строк по центру
    val maxWidth = rows.last.length // самая широкая строка (нижняя)
    // функция для центрирования строк
    def pad(row: String) {
      val padSize = (maxWidth - row.length) / 2
      for (i <- 1 to padSize) {
        print(" ")
      }
    }
    rows foreach { row => pad(row); println(row) } // построчный вывод каждого вложенного списка из основного списка (треугольника Паскаля)
  }

  println(s"Треугольник паскаля в удобочитаемом виде (количество уровней: $linesNum):")
  printPascalTriangle(triangleL, linesNum)

  /* 4. Реализуйте функцию, которая принимает номер строки и номер элемента в этой строке треугольника Паскаля
     и возвращает значение этого элемента.
  */

  val rowVal = linesNum - 1 // задаём номер строки (не забывать, что нумерация начинается с 0)
  val numVal = linesNum / 2 // задаём номер элемента в строке (нумерация - с 0), не может быть больше номера строки

  def pasTriElemSearch(num: Int, row: Int): Int = {
    if(num > row) throw new RuntimeException
    if (num <= 0 || num == row) {return 1}
    return pasTriElemSearch(num, row - 1) + pasTriElemSearch(num - 1, row - 1)
  }

  println(s"В строке $rowVal элементом под номером $numVal является число: \n ${pasTriElemSearch(numVal, rowVal)}")

  // 5. Создайте функцию, которая находит сумму элементов в указанной строке треугольника Паскаля.

  // Задаём номер строки, сумму элементов которой необходимо вычислить
  // (помнить, что номер строки на 1 меньше количества уровней, т.к. нумерация идёт с 0)
  val rowForSum = linesNum -1
  def triPasRowSum(rowNum: Int) {
    // Заданная строка треугольника Паскаля в виде списка (запись в переменную triangleRow)
    val triangleRow = pascalTriangleList(basicLine).take(rowForSum + 1).toList(rowForSum)
    //println(s"Строка номер ${rowForSum} треугольника Паскаля в виде списка: \n $triangleRow")
    println(s"Сумма элементов строки номер ${rowForSum} треугольника Паскаля: \n ${triangleRow.sum}")
  }
//  println(s"Сумма элементов строки номер ${rowForSum} треугольника Паскаля: \n ${triPasRowSum(rowForSum)}")
  triPasRowSum(rowForSum)

  /* 6. Реализуйте функцию, которая проверяет, является ли треугольник Паскаля симметричным
      (то есть, числа симметричны относительно вертикальной оси).
  */

  def isTriPasSymmetrical(numRows: Int) {
    // Треугольник Паскаля
    val triPas = pascalTriangleList(basicLine).take(numRows).toList
  //  println(s"Треугольник Паскаля в виде списка списков: \n ${triPas}")

    // Решение с использованием циклов
    var isTriPas: Boolean = false
    var i = 0
    while (i < triPas.length) {
      //    println(triPas(i))
      //    println(triPas(i).length/2)
      var j = 0
      var lst: List[Boolean] = List()
      var k = false
      var falseV = ""
      while (j < triPas(i).length / 2) {
        //      println(triPas(i)(j))
        //      println(triPas(i)(triPas(i).length - 1 -j))
        k = (triPas(i)(j) == triPas(i)(triPas(i).length - 1 - j))
        if (k == true) lst = k :: lst else falseV = s"Строка $i несимметрична"
        j += 1
      }
      if (falseV == s"Строка $i несимметрична") {
        println(s"Строка $i несимметрична")
        isTriPas = false
      } else {
//        println(s"Строка $i симметрична")
        isTriPas = true
      }
      i += 1
    }
    if (isTriPas) println("Треугольник Паскаля симметричен")
    else println("Треугольник Паскаля несимметричен")
  }

  isTriPasSymmetrical(linesNum)

}
