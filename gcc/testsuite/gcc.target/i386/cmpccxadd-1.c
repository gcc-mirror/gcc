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
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_O);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_O);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NO);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NO);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_B);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_B);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NB);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NB);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_Z);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_Z);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NZ);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NZ);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_BE);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_BE);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NBE);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NBE);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_S);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_S);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NS);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NS);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_P);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_P);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NP);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NP);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_L);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_L);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NL);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NL);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_LE);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_LE);
  b = _cmpccxadd_epi32 (a, b, c, _CMPCCX_NLE);
  e = _cmpccxadd_epi64 (d, e, f, _CMPCCX_NLE);
}
