/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -mfpmath=387" } */

void
__attribute__((noinline))
test (long double x, long double y)
{
  long double ldbl_n = __builtin_atan2l (x, y);
  long double ldbl_s = __builtin_atan2l (y, x);  // arguments swapped

  if (ldbl_n < 1.L ||  1.L < ldbl_s)
    __builtin_abort ();

  double dbl_n = __builtin_atan2  (x, y);
  double dbl_s = __builtin_atan2  (y, x);  // arguments swapped

  if (dbl_n < 1. ||  1. < dbl_s)
    __builtin_abort ();
}

int
main ()
{
  long double x = 0.922766L;
  long double y = 0.080466L;

  test (x, y);

  return 0;
}
