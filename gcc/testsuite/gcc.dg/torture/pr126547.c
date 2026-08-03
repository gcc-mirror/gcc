/* PR tree-optimization/126547 */
/* { dg-do run } */

#if __LDBL_HAS_INFINITY__
[[gnu::noipa]] static int
foo (long double a1, long double a2)
{
  if (a1 >= 1.0 && a1 <= 8.0)
    {
      long double d = a1 / a2;
      int n = 0;
      if (d >= __builtin_infl ())
	n += 1;
      if (a2 > 0.0L)
	n += 2;
      return n;
    }
  else
    return -1;
}
#endif

int
main ()
{
#if __LDBL_HAS_INFINITY__
  if (__builtin_isinf (1.0 / __LDBL_DENORM_MIN__)
      && foo (1.0, __LDBL_DENORM_MIN__) != 3)
    __builtin_abort ();
#endif
}
