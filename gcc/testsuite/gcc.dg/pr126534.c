/* PR tree-optimization/126534 */
/* { dg-do run } */
/* { dg-options "-O2" } */

[[gnu::noipa]] int
foo (double x)
{
  double y = __builtin_sqrt (x);
  if (y <= 5.0)
    {
      if (x > 25.0)
	return 1;
      return 2;
    }
  return 3;
}

[[gnu::noipa]] int
bar (double x)
{
  double y = __builtin_sqrt (x);
  if (y >= 5.0)
    {
      if (x < 25.0)
	return 1;
      return 2;
    }
  return 3;
}

int
main ()
{
#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_EXP__ == 1024 && __FLT_EVAL_METHOD__ == 0
  volatile double x = 0x1.9000000000001p+4;
  volatile double y = 0x1.8ffffffffffffp+4;
  if (__builtin_sqrt (x) == 5.0 && __builtin_sqrt (y) == 5.0)
    {
      if (foo (0x1.9000000000001p+4) != 1)    /* 25.000000000000004 */
	__builtin_abort ();
      if (bar (0x1.8ffffffffffffp+4) != 1)    /* 24.999999999999996 */
	__builtin_abort ();
    }
#endif
}
