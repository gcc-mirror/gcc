/* PR tree-optimization/72824 */

static inline void
foo (float *x, float value)
{
  int i;
  for (i = 0; i < 32; ++i)
    x[i] = value;
}

int
main ()
{
  float x[32];
  foo (x, -0.f);
  if (__builtin_copysignf (1.0, x[3]) != -1.0f)
    __builtin_abort ();
  return 0;
}
