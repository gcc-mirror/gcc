/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-Wabsolute-value" } */

#include <stdlib.h>
#include <inttypes.h>
#include <math.h>
#include <complex.h>

void
tst_unsigned (unsigned *pu, unsigned long *pl, unsigned long long *pll,
	      uintmax_t *pm)
{
  *pu = abs (*pu);      /* { dg-warning "taking the absolute value of unsigned type" } */
  *pl = labs (*pl);     /* { dg-warning "taking the absolute value of unsigned type" } */
  *pll = llabs (*pll);  /* { dg-warning "taking the absolute value of unsigned type" } */
  *pm = imaxabs (*pm);      /* { dg-warning "taking the absolute value of unsigned type" } */
}

void
test_int_size (long long *pll)
{
  *pll = abs (*pll);  /* { dg-warning "may cause truncation of value" } */
  *pll = abs ((int) *pll);
}

void
tst_notint (float *pf, double *pd, _Complex double *pc)
{
  *pf = abs (*pf);    /* { dg-warning "using integer absolute value function" } */
  *pd = labs (*pd);   /* { dg-warning "using integer absolute value function" } */
  *pc = abs (*pc);    /* { dg-warning "using integer absolute value function" } */
}

void
tst_notfloat (int *pi, long *pl, complex double *pc)
{
  *pi = fabsf (*pi);  /* { dg-warning "using floating point absolute value function" } */
  *pl = fabs (*pl);   /* { dg-warning "using floating point absolute value function" } */
  *pc = fabs (*pc);   /* { dg-warning "using floating point absolute value function" } */
}

void
tst_float_size (double *pd, long double *pld)
{
  *pd = fabsf (*pd);   /* { dg-warning "may cause truncation of value" } */
  *pld = fabs (*pld);  /* { dg-warning "may cause truncation of value" "fabs trunc" { target { large_long_double } } } */
  *pld = fabs ((double) *pld);
}

void tst_notcomplex (int *pi, long *pl, long double *pld)
{
  *pi = cabs (*pi);   /* { dg-warning "using complex absolute value function" } */
  *pl = cabs (*pl);   /* { dg-warning "using complex absolute value function" } */
  *pld = cabsl (*pld);/* { dg-warning "using complex absolute value function" } */
}

void tst_cplx_size (complex double *pcd, complex long double *pcld)
{
  *pcd = cabsf (*pcd);   /* { dg-warning "may cause truncation of value" } */
  *pcld = cabs (*pcld);  /* { dg-warning "may cause truncation of value" "cabs trunc" { target { large_long_double } } } */
  *pcld = cabs ((complex double) *pcld);
}




