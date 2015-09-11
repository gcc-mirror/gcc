// PR sanitizer/66190
// { dg-do run }
// { dg-options "-fsanitize=null -std=c++11" }

int
main ()
{
  static int *a;
  static int &b = *a;
  static int &c (*a);
  static int &d {*a};
  return 0;
}

// { dg-output "reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*reference binding to null pointer of type 'int'" }
