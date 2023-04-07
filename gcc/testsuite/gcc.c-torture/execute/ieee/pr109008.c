/* PR tree-optimization/109008 */

__attribute__((noipa)) double
foo (double eps)
{
  double d = 1. + eps;
  if (d == 1.)
    return eps;
  return 0.0;
}

int
main ()
{
  if (foo (__DBL_EPSILON__ / 8.0) == 0.0)
    __builtin_abort ();
  return 0;
}
