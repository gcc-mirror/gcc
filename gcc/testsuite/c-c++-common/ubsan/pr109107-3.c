/* PR sanitizer/109107 */
/* { dg-do run { target int32 } } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

#define INT_MIN (-__INT_MAX__ - 1)
const int a = INT_MIN;
const int b = 40;
int d = 1;

__attribute__((noipa)) int
foo ()
{
  int c = a - d + (int) (short) b;
  return c;
}

int
main ()
{
  foo ();
  return 0;
}

/* { dg-output "signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: 2147483647 \\+ 40 cannot be represented in type 'int'" } */
