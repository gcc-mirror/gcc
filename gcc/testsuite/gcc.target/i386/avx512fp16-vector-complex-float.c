/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mavx512fp16 -mavx512vl -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "vfmadd\[123]*ph\[ \\t\]"} } */
/* { dg-final { scan-assembler-not "vfmadd\[123]*sh\[ \\t\]"} } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-times "vfcmulcph\[ \\t\]" 1 } } */

#include<complex.h>
#define TYPE _Float16
#define N 16

void fma0 (_Complex TYPE *a, _Complex TYPE *b,
           _Complex TYPE * __restrict c)
{
  for (int i = 0; i < N; i++)
    c[i] += a[i] * b[i];
}

void fmaconj (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	      _Complex TYPE c[restrict N])
{
  for (int i = 0; i < N; i++)
    c[i] += a[i] * ~b[i];
}

void fmul (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	   _Complex TYPE c[restrict N])
{
  for (int i = 0; i < N; i++)
    c[i] = a[i] * b[i];
}

void fmulconj (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
	       _Complex TYPE c[restrict N])
{
  for (int i = 0; i < N; i++)
    c[i] = a[i] * ~b[i];
}
