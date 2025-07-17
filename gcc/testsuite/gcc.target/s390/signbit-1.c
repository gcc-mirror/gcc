/* { dg-do run } */
/* { dg-options "-O2 -march=z900 -save-temps" } */
/* { dg-final { scan-assembler-times {\ttceb\t} 2 } } */
/* { dg-final { scan-assembler-times {\ttcdb\t} 2 } } */
/* { dg-final { scan-assembler-times {\ttcxb\t} 2 } } */

/* Binary Floating-Point */

__attribute__ ((noipa))
int signbit_float_reg (float x) { return __builtin_signbit (x); }
__attribute__ ((noipa))
int signbit_float_mem (float *x) { return __builtin_signbit (*x); }
__attribute__ ((noipa))
int signbit_double_reg (double x) { return __builtin_signbit (x); }
__attribute__ ((noipa))
int signbit_double_mem (double *x) { return __builtin_signbit (*x); }

__attribute__ ((noipa))
int
signbit_longdouble_reg (long double x)
{
  __asm__ ("" : "+f" (x));
  return __builtin_signbit (x);
}

__attribute__ ((noipa))
int signbit_longdouble_mem (long double *x) { return __builtin_signbit (*x); }

#include "signbit.h"
TEST (float, float, __builtin_inff(), __builtin_nanf("42"), 0.f, 42.f)
TEST (double, double, __builtin_inf(), __builtin_nan("42"), 0., 42.)
TEST (longdouble, long double, __builtin_infl(), __builtin_nanl("42"), 0.L, 42.L)

int
main (void)
{
  test_float ();
  test_double ();
  test_longdouble ();
}
