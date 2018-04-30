/* PR tree-optimization/81556 */

unsigned long long int b = 0xb82ff73c5c020599ULL;
unsigned long long int c = 0xd4e8188733a29d8eULL;
unsigned long long int d = 2, f = 1, g = 0, h = 0;
unsigned long long int e = 0xf27771784749f32bULL;

__attribute__((noinline, noclone)) void
foo (void)
{
  _Bool a = d > 1;
  g = f % ((d > 1) << 9);
  h = a & (e & (a & b & c));
}

int
main ()
{
  foo ();
  if (g != 1 || h != 0)
    __builtin_abort ();
  return 0;
}
