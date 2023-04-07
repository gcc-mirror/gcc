/* PR tree-optimization/109386 */

static inline float
foo (float x, float y)
{
  float u = __builtin_fabsf (x);
  float v = __builtin_fabsf (y);
  if (!(u >= v))
    {
      if (__builtin_isinf (v)) return v;
      if (__builtin_isinf (u)) return u;
    }
  return 42.0f;
}

int
main ()
{
  if (!__builtin_isinf (foo (__builtin_inff (), __builtin_nanf (""))))
    __builtin_abort ();
}
