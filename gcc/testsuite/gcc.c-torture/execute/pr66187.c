/* PR tree-optimization/66187 */

int a = 1, e = -1;
short b, f;

int
main ()
{
  f = e;
  int g = b < 0 ? 0 : f + b;
  if ((g & -4) < 0)
    a = 0;
  if (a)
    __builtin_abort ();
  return 0;
}
