/* PR tree-optimization/70127 */

struct S { int f; signed int g : 2; } a[1], c = {5, 1}, d;
short b;

__attribute__((noinline, noclone)) void
foo (int x)
{
  if (x != 1)
    __builtin_abort ();
}

int
main ()
{
  while (b++ <= 0)
    {
      struct S e = {1, 1};
      d = e = a[0] = c;
    }
  foo (a[0].g);
  return 0;
}
