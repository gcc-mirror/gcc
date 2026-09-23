/* PR tree-optimization/126464 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-add-options ieee } */
/* { dg-skip-if "not IEEE float" { "pdp11-*-*" } } */

[[gnu::noipa]] double
foo (double x)
{
  float y = (float) x;

  if (y == -__builtin_inff ())
    return x * 0.5;
  return y;
}

[[gnu::noipa]] long double
bar (long double x)
{
  double y = (double) x;

  if (y == __builtin_inf ())
    return x * 0.5L;
  return y;
}

[[gnu::noipa]] double
baz (double x)
{
  float y = (float) x;

  if (y == __builtin_inff ())
    return x * 0.5;
  return y;
}

[[gnu::noipa]] long double
qux (long double x)
{
  double y = (double) x;

  if (y == -__builtin_inf ())
    return x * 0.5L;
  return y;
}

int
main ()
{
#if __DBL_MAX_10_EXP__ >= 301
  if (!__builtin_isinf ((double) 1e300)
      && __builtin_isinf ((float) 1e300)
      && (foo (-1e300) != -5e299
	  || baz (1e300) != 5e299))
    __builtin_abort ();
#endif

#if __LDBL_MAX_10_EXP__ >= 4001
  if (!__builtin_isinf ((long double) 1e4000L)
      && __builtin_isinf ((double) 1e4000L)
      && (bar (1e4000L) != 5e3999L
	  || qux (-1e4000L) != -5e3999L))
    __builtin_abort ();
#endif
}
