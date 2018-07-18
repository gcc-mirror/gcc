/* PR tree-optimization/85331 */

typedef double V __attribute__((vector_size (2 * sizeof (double))));
typedef long long W __attribute__((vector_size (2 * sizeof (long long))));

__attribute__((noipa)) void
foo (V *r)
{
  V y = { 1.0, 2.0 };
  W m = { 10000000001LL, 0LL };
  *r = __builtin_shuffle (y, m);
}

int
main ()
{
  V r;
  foo (&r);
  if (r[0] != 2.0 || r[1] != 1.0)
    __builtin_abort ();
  return 0;
}
