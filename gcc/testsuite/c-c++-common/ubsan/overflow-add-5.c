/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

int __attribute__ ((noinline))
foo (int i, int j)
{
  return (i + j) - (i | j);
}

/* { dg-output "signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 - 2147483647 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */

int __attribute__ ((noinline))
bar (int i, int j)
{
  return (i + j) - (i & j);
}

/* { dg-output "\[^\n\r]*signed integer overflow: 2147483647 \\+ 1 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*signed integer overflow: -2147483648 - 1 cannot be represented in type 'int'" } */

int
main ()
{
  int r = foo (__INT_MAX__, 1);
  asm volatile ("" : "+g" (r));
  r = bar (__INT_MAX__, 1);
  asm volatile ("" : "+g" (r));
  return 0;
}
