/* PR tree-optimization/58364 */

int a = 1, b, c;

int
foo (int x)
{
  return x < 0 ? 1 : x;
}

int
main ()
{
  if (foo (a > c == (b = 0)))
    __builtin_abort ();
  return 0;
}
