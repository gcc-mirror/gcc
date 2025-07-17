/* { dg-do run { target lp64 } } */
/* { dg-options "-O2 -march=z10 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Binary Floating-Point */

/*
** signbit_float_reg:
**	lgdr	(%r[0-9]+),%f0
**	srlg	(%r[0-9]+),\1,63
**	lgfr	%r2,\2
**	br	%r14
*/
__attribute__ ((noipa))
int signbit_float_reg (float x) { return __builtin_signbit (x); }

/*
** signbit_float_mem:
**    l	(%r[0-9]+),0\(%r2\)
**    srl	\1,31
**    lgfr	%r2,\1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_float_mem (float *x) { return __builtin_signbit (*x); }

/*
** signbit_double_reg:
**    lgdr	(%r[0-9]+),%f0
**    srlg	%r2,\1,63
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_double_reg (double x) { return __builtin_signbit (x); }

/*
** signbit_double_mem:
**    lg	(%r[0-9]+),0\(%r2\)
**    srlg	%r2,\1,63
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_double_mem (double *x) { return __builtin_signbit (*x); }

/*
** signbit_longdouble_reg:
**     ld	%f0,0\(%r2\)
**     ld	%f2,8\(%r2\)
**     lgdr	(%r[0-9]+),%f0
**     srlg	%r2,\1,63
**     br	%r14
*/
__attribute__ ((noipa))
int
signbit_longdouble_reg (long double x)
{
  __asm__ ("" : "+f" (x));
  return __builtin_signbit (x);
}

/*
** signbit_longdouble_mem:
**      lg	(%r[0-9]+),0\(%r2\)
**      srlg	%r2,\1,63
**      br	%r14
*/
__attribute__ ((noipa))
int signbit_longdouble_mem (long double *x) { return __builtin_signbit (*x); }

/* Decimal Floating-Point */

/*
** signbit_dec32_reg:
**	lgdr	(%r[0-9]+),%f0
**	srlg	(%r[0-9]+),\1,63
**	lgfr	%r2,\2
**	br	%r14
*/
__attribute__ ((noipa))
int signbit_dec32_reg (_Decimal32 x) { return __builtin_signbit (x); }

/*
** signbit_dec32_mem:
**    l	(%r[0-9]+),0\(%r2\)
**    srl	\1,31
**    lgfr	%r2,\1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_dec32_mem (_Decimal32 *x) { return __builtin_signbit (*x); }

/*
** signbit_dec64_reg:
**    lgdr	(%r[0-9]+),%f0
**    srlg	%r2,\1,63
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_dec64_reg (_Decimal64 x) { return __builtin_signbit (x); }

/*
** signbit_dec64_mem:
**    lg	(%r[0-9]+),0\(%r2\)
**    srlg	%r2,\1,63
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_dec64_mem (_Decimal64 *x) { return __builtin_signbit (*x); }

/*
** signbit_dec128_reg:
**     ld	%f0,0\(%r2\)
**     ld	%f2,8\(%r2\)
**     lgdr	(%r[0-9]+),%f0
**     srlg	%r2,\1,63
**     br	%r14
*/
__attribute__ ((noipa))
int
signbit_dec128_reg (_Decimal128 x)
{
  __asm__ ("" : "+f" (x));
  return __builtin_signbit (x);
}

/*
** signbit_dec128_mem:
**      lg	(%r[0-9]+),0\(%r2\)
**      srlg	%r2,\1,63
**      br	%r14
*/
__attribute__ ((noipa))
int signbit_dec128_mem (_Decimal128 *x) { return __builtin_signbit (*x); }

#include "signbit.h"
TEST (float, float, __builtin_inff(), __builtin_nanf("42"), 0.f, 42.f)
TEST (double, double, __builtin_inf(), __builtin_nan("42"), 0., 42.)
TEST (longdouble, long double, __builtin_infl(), __builtin_nanl("42"), 0.L, 42.L)
TEST (dec32, _Decimal32, __builtin_infd32(), __builtin_nand32("42"), 0.df, 42.df)
TEST (dec64, _Decimal64, __builtin_infd64(), __builtin_nand64("42"), 0.dd, 42.dd)
TEST (dec128, _Decimal128, __builtin_infd128(), __builtin_nand128("42"), 0.dl, 42.dl)

int
main (void)
{
  test_float ();
  test_double ();
  test_longdouble ();
  test_dec32 ();
  test_dec64 ();
  test_dec128 ();
}
