/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mcmpccxadd" } */
/* { dg-final { scan-assembler-times "cmpoxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnoxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpbxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnbxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpzxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnzxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpbexadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnbexadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpsxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnsxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmppxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnpxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmplxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnlxadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmplexadd\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-times "cmpnlexadd\[ \\t\]" 2 } } */
#include <x86gprintrin.h>

int *a;
int b, c;
long long *d;
long long e, f;

void extern
cmpccxadd_test(void)
{
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_O);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_O);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NO);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NO);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_B);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_B);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NB);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NB);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_Z);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_Z);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NZ);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NZ);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_BE);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_BE);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NBE);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NBE);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_S);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_S);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NS);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NS);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_P);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_P);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NP);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NP);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_L);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_L);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NL);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NL);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_LE);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_LE);
  b = __cmpccxadd_epi32 (a, b, c, _CMPCCX_NLE);
  e = __cmpccxadd_epi64 (d, e, f, _CMPCCX_NLE);
}
