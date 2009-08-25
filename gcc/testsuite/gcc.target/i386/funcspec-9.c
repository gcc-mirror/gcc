/* Test whether using target specific options, we can generate SSE5 code.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=k8 -mfpmath=sse -msse2" } */

extern void exit (int);

#ifdef __SSE4A__
#warning "__SSE4A__ should not be defined before #pragma GCC target."
#endif

#pragma GCC push_options
#pragma GCC target ("sse4a")

#ifndef __SSE4A__
#warning "__SSE4A__ should have be defined after #pragma GCC target."
#endif

float
flt_mul_add (float a, float b, float c)
{
  return (a * b) + c;
}

#pragma GCC pop_options
#ifdef __SSE4A__
#warning "__SSE4A__ should not be defined after #pragma GCC pop target."
#endif

double
dbl_mul_add (double a, double b, double c)
{
  return (a * b) + c;
}

/* We used to generate fused-madd with SSE5 support, but don't do that anymore.  */
/* { dg-final { scan-assembler "addsd" } } */
