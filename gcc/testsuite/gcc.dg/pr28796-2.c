/* { dg-do run } */
/* { dg-options "-O2 -funsafe-math-optimizations -fno-finite-math-only" } */
/* { dg-options "-mieee -O2 -funsafe-math-optimizations -fno-finite-math-only" { target alpha*-*-* } } */

extern void abort (void);

void __attribute__ ((__noinline__))
foo_1 (float f, double d, long double ld,
       int res_unord, int res_isnan, int res_isinf,
       int res_isfin, int res_isnorm)
{
  if (__builtin_isunordered (f, 0) != res_unord)
    abort ();
  if (__builtin_isunordered (0, f) != res_unord)
    abort ();
  if (__builtin_isunordered (d, 0) != res_unord)
    abort ();
  if (__builtin_isunordered (0, d) != res_unord)
    abort ();
  if (__builtin_isunordered (ld, 0) != res_unord)
    abort ();
  if (__builtin_isunordered (0, ld) != res_unord)
    abort ();

  if (__builtin_isnan (f) != res_isnan)
    abort ();
  if (__builtin_isnan (d) != res_isnan)
    abort ();
  if (__builtin_isnan (ld) != res_isnan)
    abort ();
  if (__builtin_isnanf (f) != res_isnan)
    abort ();
  if (__builtin_isnanl (ld) != res_isnan)
    abort ();

  if (__builtin_isinf (f) != res_isinf)
    abort ();
  if (__builtin_isinf (d) != res_isinf)
    abort ();
  if (__builtin_isinf (ld) != res_isinf)
    abort ();
  if (__builtin_isinff (f) != res_isinf)
    abort ();
  if (__builtin_isinfl (ld) != res_isinf)
    abort ();

  if (__builtin_isnormal (f) != res_isnorm)
    abort ();
  if (__builtin_isnormal (d) != res_isnorm)
    abort ();
  if (__builtin_isnormal (ld) != res_isnorm)
    abort ();

  if (__builtin_isfinite (f) != res_isfin)
    abort ();
  if (__builtin_isfinite (d) != res_isfin)
    abort ();
  if (__builtin_isfinite (ld) != res_isfin)
    abort ();

  if (__builtin_finitef (f) != res_isfin)
    abort ();
  if (__builtin_finite (f) != res_isfin)
    abort ();
  if (__builtin_finite (d) != res_isfin)
    abort ();
  if (__builtin_finitel (d) != res_isfin)
    abort ();
  if (__builtin_finitel (ld) != res_isfin)
    abort ();
}

void __attribute__ ((__noinline__))
foo (float f, double d, long double ld,
     int res_unord, int res_isnan, int res_isinf,
     int res_isfin, int res_isnorm)
{
  foo_1 (f, d, ld, res_unord, res_isnan, res_isinf, res_isfin, res_isnorm);
  /* Try all values negative as well.  */
  foo_1 (-f, -d, -ld, res_unord, res_isnan, res_isinf, res_isfin, res_isnorm);
}

int main()
{
  float f;
  double d;
  long double ld;
  
  f = __builtin_nanf(""); d = __builtin_nan(""); ld = __builtin_nanl("");
  foo(f, d, ld, /*unord=*/ 1, /*isnan=*/ 1, /*isinf=*/ 0, /*isfin=*/ 0, /*isnorm=*/ 0);

  f = __builtin_inff(); d = __builtin_inf(); ld = __builtin_infl();
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 1, /*isfin=*/ 0, /*isnorm=*/ 0);

  f = 0; d = 0; ld = 0;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 0);

  f = 1; d = 1; ld = 1;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  f = __FLT_MIN__; d = __DBL_MIN__; ld = __LDBL_MIN__;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  f = __FLT_MIN__/2; d = __DBL_MIN__/2; ld = __LDBL_MIN__/2;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 0);

  f = __FLT_MAX__; d = __DBL_MAX__; ld = __LDBL_MAX__;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 0, /*isfin=*/ 1, /*isnorm=*/ 1);

  f = __FLT_MAX__*2; d = __DBL_MAX__*2; ld = __LDBL_MAX__*2;
  foo(f, d, ld, /*unord=*/ 0, /*isnan=*/ 0, /*isinf=*/ 1, /*isfin=*/ 0, /*isnorm=*/ 0);

  return 0;
}
