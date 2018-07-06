/* PR tree-optimization/85529 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-ssa-phiopt" } */

__attribute__((noinline, noclone)) int
foo (int x)
{
  x &= 31;
  x -= 25;
  x *= 2;
  if (x < 0)
    return 1;
  int y = x >> 2;
  if (x >= y)
    return 1;
  return 0;
}

int
main ()
{
  int i;
  for (i = 0; i < 63; i++)
    if (foo (i) != 1)
      __builtin_abort ();
  return 0;
}
