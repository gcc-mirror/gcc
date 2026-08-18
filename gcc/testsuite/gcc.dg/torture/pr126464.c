/* PR tree-optimization/126464 */
/* { dg-do run } */
/* { dg-add-options ieee } */

#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_10_EXP__ == 308 \
    && __DBL_HAS_INFINITY__ && __FLT_EVAL_METHOD__ == 0
[[gnu::noipa]] double
foo (double x, double y)
{
  if (y >= -1e304 && y <= -1e300)
    {
      if (x + y == -__builtin_inf ())
	return x * 0.5;
    }
  return x;
}
#endif

int
main ()
{
#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_10_EXP__ == 308 \
    && __DBL_HAS_INFINITY__ && __FLT_EVAL_METHOD__ == 0
  if (foo (-__builtin_inf (), -1e303) != -__builtin_inf ())
    __builtin_abort ();
  if (foo (-1.797693134862e308, -1e303) != -1.797693134862e308 / 2.0)
    __builtin_abort ();
#endif
}
