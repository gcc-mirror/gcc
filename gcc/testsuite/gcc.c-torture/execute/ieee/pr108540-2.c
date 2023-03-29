/* PR tree-optimization/108540 */

__attribute__((noipa)) int
foo (int x, double d)
{
  if (x == 42)
    d = -0.0;
  if (d == 0.0)
    return 42;
  return 12;
}

int
main ()
{
  if (foo (42, 5.0) != 42
      || foo (42, 0.0) != 42
      || foo (42, -0.0) != 42
      || foo (10, 5.0) != 12
      || foo (10, 0.0) != 42
      || foo (10, -0.0) != 42)
    __builtin_abort ();
}
