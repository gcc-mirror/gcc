/* Test whether using target specific options, we can generate FMA4 code.  */
/* { dg-do compile } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=k8" } } */
/* { dg-options "-O2 -march=k8 -mfpmath=sse -msse2" } */

extern void exit (int);

#ifdef __FMA4__
#warning "__FMA4__ should not be defined before #pragma GCC target."
#endif

#pragma GCC push_options
#pragma GCC target ("fma4")

#ifndef __FMA4__
#warning "__FMA4__ should have be defined after #pragma GCC target."
#endif

float
flt_mul_add (float a, float b, float c)
{
  return (a * b) + c;
}

#pragma GCC pop_options
#ifdef __FMA4__
#warning "__FMA4__ should not be defined after #pragma GCC pop target."
#endif

double
dbl_mul_add (double a, double b, double c)
{
  return (a * b) + c;
}

/* { dg-final { scan-assembler "vfmaddss" } } */
/* { dg-final { scan-assembler "addsd" } } */
