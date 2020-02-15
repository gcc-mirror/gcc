/* PR tree-optimization/93744 */

typedef int I;

int
main ()
{
  int a = 0;
  I b = 0;
  (a > 0) * (b |= 2);
  if (b != 2)
    __builtin_abort ();
  return 0;
}
