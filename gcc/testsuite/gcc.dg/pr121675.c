/* PR rtl-optimization/121675 */
/* { dg-do run { target int32plus } } */
/* { dg-options "-O3" } */

int a, c, d, e;

int
foo (volatile int f, int g)
{
  f = -2;
  if (!f)
    a = 1;
  c = 11 / f - g + 2;
  g = c + 2;
  if (!(1 / c + (g - 1)))
    a = 1;
  return a + f;
}

int
main ()
{
  do
    {
      e = (foo (1, -1) - 30885397) % 2 - 3;
      d = foo (1, -1) - 1430885400;
    }
  while (a + 1 == 0);
  if (d + e != -1430885406)
    __builtin_abort ();
  return 0;
}
