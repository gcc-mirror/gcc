/* PR tree-optimization/109008 */
/* { dg-do run } */
/* { dg-options "-O2 -ffinite-math-only -fexcess-precision=standard" } */

__attribute__((noipa)) double
foo (double eps)
{
  double d = __DBL_MAX__ + eps;
  if (d == __DBL_MAX__)
    if (eps > 16.0)
      return eps;
  return 0.0;
}

int
main ()
{
#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_EXP__ == 1024 && __DBL_MIN_EXP__ == -1021 \
    && __FLT_EVAL_METHOD__ == 0
  if (foo (0x0.8p+970) == 0.0)
    __builtin_abort ();
  if (foo (32.0) == 0.0)
    __builtin_abort ();
#endif
  return 0;
}
