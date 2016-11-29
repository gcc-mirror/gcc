/* PR tree-optimization/72824 */

typedef float V __attribute__((vector_size (4 * sizeof (float))));

static inline void
foo (V *x, V value)
{
  int i;
  for (i = 0; i < 32; ++i)
    x[i] = value;
}

int
main ()
{
  V x[32];
  foo (x, (V) { 0.f, -0.f, 0.f, -0.f });
  if (__builtin_copysignf (1.0, x[3][1]) != -1.0f)
    __builtin_abort ();
  return 0;
}
