/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final {scan-assembler-times "vpsrlw\[ \\t\]" 1 } } */
/* { dg-final {scan-assembler-times "vpsllw\[ \\t\]" 1 } } */
/* { dg-final {scan-assembler-times "vpsraw\[ \\t\]" 1 } } */
/* { dg-final {scan-assembler-not "vpbroadcastw\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpsrlvw\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpsllvw\[ \\t\]" } } */
/* { dg-final {scan-assembler-not "vpsravw\[ \\t\]" } } */
#include <immintrin.h>

volatile __m512i a, b;

void
foo()
{
  b = _mm512_srlv_epi16 (a, _mm512_set1_epi16 (3));
  b = _mm512_sllv_epi16 (a, _mm512_set1_epi16 (4));
  b = _mm512_srav_epi16 (a, _mm512_set1_epi16 (5));
}
