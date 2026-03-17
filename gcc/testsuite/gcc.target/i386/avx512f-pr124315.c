/* PR target/124315 */
/* { dg-do compile { target masm_intel } } */
/* { dg-options "-O2 -mavx512f -masm=intel" } */
/* { dg-final { scan-assembler "\tvfmadd231sd\txmm0{k\[0-7]}, xmm1, xmm2, {ru-sae}" } } */
/* { dg-final { scan-assembler "\tvfmsub231sd\txmm0{k\[0-7]}, xmm1, xmm2, {ru-sae}" } } */
/* { dg-final { scan-assembler "\tvfnmadd231sd\txmm0{k\[0-7]}, xmm1, xmm2, {ru-sae}" } } */
/* { dg-final { scan-assembler "\tvfnmsub231sd\txmm0{k\[0-7]}, xmm1, xmm2, {ru-sae}" } } */

#include <x86intrin.h>

__m128d
foo (__m128d b, __m128d w, __m128d a,  __mmask8 u)
{
  return _mm_mask3_fmadd_round_sd (w, a, b, u, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}

__m128d
bar (__m128d b, __m128d w, __m128d a, __mmask8 u)
{
  return _mm_mask3_fmsub_round_sd (w, a, b, u, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}

__m128d
baz (__m128d b, __m128d w, __m128d a,  __mmask8 u)
{
  return _mm_mask3_fnmadd_round_sd (w, a, b, u, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}

__m128d
qux (__m128d b, __m128d w, __m128d a,  __mmask8 u)
{
  return _mm_mask3_fnmsub_round_sd (w, a, b, u, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}
