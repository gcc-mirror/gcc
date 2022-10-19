/* PR tree-optimization/106990 */
/* { dg-do run { target int32 } } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

__attribute__((noipa)) int
foo (void)
{
  int x = -1956816001;
  int y = 1999200512;
  return ~x - ~y;
}

__attribute__((noipa)) int
bar (void)
{
  int x = -__INT_MAX__ - 1;
  return -x & 1;
}

int
main ()
{
  foo ();
  bar ();
  return 0;
}

/* { dg-output "signed integer overflow: 1956816000 - -1999200513 cannot be represented in type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*negation of -2147483648 cannot be represented in type 'int'; cast to an unsigned type to negate this value to itself" } */
