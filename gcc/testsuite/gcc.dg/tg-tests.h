/* Test various type-generic builtins by calling "main_tests()".  */

void __attribute__ ((__noinline__))
foo_1 (float f, double d, long double ld,
       int res_unord, int res_isnan, int res_isinf,
       int res_isfin, int res_isnorm)
{
  if (__builtin_isunordered (f, 0) != res_unord)
    __builtin_abort ();
  if (__builtin_isunordered (0, f) != res_unord)
    __builtin_abort ();
  if (__builtin_isunordered (d, 0) != res_unord)
    __builtin_abort ();
  if (__builtin_isunordered (0, d) != res_unord)
    __builtin_abort ();
  if (__builtin_isunordered (ld, 0) != res_unord)
    __builtin_abort ();
  if (__builtin_isunordered (0, ld) != res_unord)
    __builtin_abort ();

  if (__builtin_isnan (f) != res_isnan)
    __builtin_abort ();
  if (__builtin_isnan (d) != res_isnan)
    __builtin_abort ();
  if (__builtin_isnan (ld) != res_isnan)
    __builtin_abort ();
  if (__builtin_isnanf (f) != res_isnan)
    __builtin_abort ();
  if (__builtin_isnanl (ld) != res_isnan)
    __builtin_abort ();

  if (__builtin_isinf (f) != res_isinf)
    __builtin_abort ();
  if (__builtin_isinf (d) != res_isinf)
    __builtin_abort ();
  if (__builtin_isinf (ld) != res_isinf)
    __builtin_abort ();
  if (__builtin_isinff (f) != res_isinf)
    __builtin_abort ();
  if (__builtin_isinfl (ld) != res_isinf)
    __builtin_abort ();

  if (__builtin_isnormal (f) != res_isnorm)
    __builtin_abort ();
  if (__builtin_isnormal (d) != res_isnorm)
    __builtin_abort ();
  if (__builtin_isnormal (ld) != res_isnorm)
    __builtin_abort ();

  if (__builtin_isfinite (f) != res_isfin)
    __builtin_abort ();
  if (__builtin_isfinite (d) != res_isfin)
    __builtin_abort ();
  if (__builtin_isfinite (ld) != res_isfin)
    __builtin_abort ();

  if (__builtin_finitef (f) != res_isfin)
    __builtin_abort ();
  if (__builtin_finite (f) != res_isfin)
    __builtin_abort ();
  if (__builtin_finite (d) != res_isfin)
    __builtin_abort ();
  if (__builtin_finitel (d) != res_isfin)
    __builtin_abort ();
  if (__builtin_finitel (ld) != res_isfin)
    __builtin_abort ();
}

void __attribute__ ((__noinline__))
foo (float f, double d, long double ld,
     int res_unord, int res_isnan, int res_isinf,
     int res_isfin, int res_isnorm)
{
  foo_1 (f, d, ld, res_unord, res_isnan, res_isinf, res_isfin, res_isnorm);
  /* Try all the values negated as well.  */
  foo_1 (-f, -d, -ld, res_unord, res_isnan, res_isinf, res_isfin, res_isnorm);
}

int __attribute__ ((__noinline__))
main_tests (void)
{
  float f;
  double d;
  long double ld;
  
  /* Test NaN.  */
  f = __builtin_nanf(""); d = __builtin_nan(""); ld = __builtin_nanl("");
  foo(f, d, ld, /*unord=*/ 1, /*isnan=*/ 1, /*isinf=*/ 0, /*isfin=*/ 0, /*isnorm=*/ 0);

  /* Test infinity.  */
  f = __builtin_inff(); d = __builtin_inf(); ld = __builtin_infl();
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 1, /*isfin=*/ 0, /*isnorm=*/ 0);

  /* Test zero.  */
  f = 0; d = 0; ld = 0;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 0);

  /* Test one.  */
  f = 1; d = 1; ld = 1;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  /* Test minimum values.  */
  f = __FLT_MIN__; d = __DBL_MIN__; ld = __LDBL_MIN__;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  /* Test subnormal values.  */
  f = __FLT_MIN__/2; d = __DBL_MIN__/2; ld = __LDBL_MIN__/2;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 0);

  /* Test maximum values.  */
  f = __FLT_MAX__; d = __DBL_MAX__; ld = __LDBL_MAX__;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  /* Test overflow values.  */
  f = __FLT_MAX__*2; d = __DBL_MAX__*2; ld = __LDBL_MAX__*2;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 1, /*isfin=*/ 0, /*isnorm=*/ 0);

  return 0;
}
