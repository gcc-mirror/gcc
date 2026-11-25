/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

#define INT_MIN (-__INT_MAX__ - 1)

__attribute__((noipa)) int
f1 (int x, int y)
{
  return y + (x - y < 0 ? x - y : 0);
}

__attribute__((noipa)) int
f2 (int x, int y)
{
  return x - (x - y < 0 ? x - y : 0);
}

__attribute__((noipa)) int
f3 (int x, int y)
{
  return (x + y < y ? x + y : y) - y;
}

int
main ()
{
  f1 (INT_MIN, 1);
  f2 (INT_MIN, 1);
  f3 (INT_MIN, -1);
  return 0;
}

/* { dg-output "signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 \\+ -1 cannot be represented in type 'int'" } */
