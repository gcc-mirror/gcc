/* PR tree-optimization/126564 */
/* { dg-do run } */

[[gnu::noipa]] int
foo (int x, int y)
{
  int c = -128;
  if (x == y)
    c = 0;
  else if (x < y)
    c = -1;
  else if (x <= y)
    c = 1;
  return c > 0;
}

int
main ()
{
  int i, j;
  for (i = -3; i <= 3; i++)
    for (j = -3; j <= 3; j++)
      if (foo (i, j) != 0)
	__builtin_abort ();
}
