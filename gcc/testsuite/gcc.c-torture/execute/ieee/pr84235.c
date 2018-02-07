/* PR tree-optimization/84235 */

int
main ()
{
  double d = 1.0 / 0.0;
  _Bool b = d == d && (d - d) != (d - d);
  if (!b)
    __builtin_abort ();
  return 0;
}
