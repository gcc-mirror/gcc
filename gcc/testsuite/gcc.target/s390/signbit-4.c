/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -march=z13 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Binary Floating-Point */

/*
** signbit_float_reg:
**    vlgvf	(%r[0-9]+),%v0,0
**    risbgn	%r2,\1,64-1,128\+63,32\+1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_float_reg (float x) { return __builtin_signbit (x); }

/*
** signbit_float_mem:
**    l	(%r[0-9]+),0\(%r2\)
**    risbgn	%r2,\1,64-1,128\+63,32\+1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_float_mem (float *x) { return __builtin_signbit (*x); }

/* Decimal Floating-Point */

/*
** signbit_dec32_reg:
**    vlgvf	(%r[0-9]+),%v0,0
**    risbgn	%r2,\1,64-1,128\+63,32\+1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_dec32_reg (_Decimal32 x) { return __builtin_signbit (x); }

/*
** signbit_dec32_mem:
**    l	(%r[0-9]+),0\(%r2\)
**    risbgn	%r2,\1,64-1,128\+63,32\+1
**    br	%r14
*/
__attribute__ ((noipa))
int signbit_dec32_mem (_Decimal32 *x) { return __builtin_signbit (*x); }

#include "signbit.h"
TEST (float, float, __builtin_inff(), __builtin_nanf("42"), 0.f, 42.f)
TEST (dec32, _Decimal32, __builtin_infd32(), __builtin_nand32("42"), 0.df, 42.df)

int
main (void)
{
  test_float ();
  test_dec32 ();
}
