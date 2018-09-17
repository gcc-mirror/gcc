/* { dg-do compile } */
/* { dg-options "-Wabsolute-value" } */

#include <stdlib.h>
#include <complex.h>
#include <math.h>

void tst_decimal (_Decimal32 *p32, _Decimal64 *p64, _Decimal128 *p128)
{
  *p32 = abs(*p32);       /* { dg-warning "using integer absolute value function" } */
  *p64 = fabs(*p64);      /* { dg-warning "using floating point absolute value function" } */
  *p128 = cabsl(*p128);   /* { dg-warning "using complex absolute value function" } */
}

void tst_notdecimal (int *pi, double *pd, long double *pld, complex double *pc)
{
  *pi = __builtin_fabsd32 (*pi);   /* { dg-warning "using decimal floating point absolute value function" } */
  *pd = __builtin_fabsd64 (*pd);   /* { dg-warning "using decimal floating point absolute value function" } */
  *pld = __builtin_fabsd64 (*pld); /* { dg-warning "using decimal floating point absolute value function" } */
  *pc = __builtin_fabsd128 (*pc);  /* { dg-warning "using decimal floating point absolute value function" } */
}

void
test_size  (_Decimal64 *p64, _Decimal128 *p128)
{
  *p64 = __builtin_fabsd32 (*p64);   /* { dg-warning "may cause truncation of value" } */
  *p128 = __builtin_fabsd64 (*p128); /* { dg-warning "may cause truncation of value" } */
}
