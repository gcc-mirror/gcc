/* Test whether using target specific options, we can generate SSE5 code.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=k8 -mfpmath=sse -msse2" } */

extern void exit (int);

#ifdef __SSE5__
#warning "__SSE5__ should not be defined before #pragma GCC option."
#endif

#pragma GCC option (push)
#pragma GCC option ("sse5,fused-madd")

#ifndef __SSE5__
#warning "__SSE5__ should have be defined after #pragma GCC option."
#endif

float
flt_mul_add (float a, float b, float c)
{
  return (a * b) + c;
}

#pragma GCC option (pop)
#ifdef __SSE5__
#warning "__SSE5__ should not be defined after #pragma GCC pop option."
#endif

double
dbl_mul_add (double a, double b, double c)
{
  return (a * b) + c;
}

/* { dg-final { scan-assembler "fmaddss" } } */
/* { dg-final { scan-assembler "addsd" } } */
