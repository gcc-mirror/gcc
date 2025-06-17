/* PR tree-optimization/120677 */
/* { dg-do run { target int32plus } } */

unsigned a;
int b, e;

int
foo (int d)
{
  switch (d)
    {
    case 0:
    case 2:
      return 0;
    default:
      return 1;
    }
}

int
main ()
{
  for (b = 8; b; b--)
    if (a & 1)
      a = a >> 1 ^ 20000000;
    else
      a >>= 1;
  e = foo (0);
  if (e || a)
    __builtin_abort ();
}
