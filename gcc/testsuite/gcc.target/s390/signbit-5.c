/* { dg-do run { target lp64 } } */
/* { dg-options "-O2 -march=z14 -save-temps" } */

/*
** signbit_longdouble_reg:
**      ld	%f0,0(%r2);ld	%f2,8+0(%r2)
**      lgdr	(%r[0-9]+),%f0
**      srlg	%r2,\1,63
**      br	%r14
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

#include "signbit.h"
TEST (longdouble, long double, __builtin_infl(), __builtin_nanl("42"), 0.L, 42.L)

int
main (void)
{
  test_longdouble ();
}
