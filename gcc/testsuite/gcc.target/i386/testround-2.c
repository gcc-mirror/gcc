/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O0 -mavx512f" } */

#include <x86intrin.h>

long long l;
unsigned long long ul;
__m128d m128d;
__m128  m128;

void
test_round_64 (void)
{
  m128d = _mm_cvt_roundu64_sd (m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_cvt_roundi64_sd (m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_cvt_roundu64_ss (m128, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundi64_ss (m128, 4, 7); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvt_roundss_u64 (m128, 7); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvt_roundss_i64 (m128, 7); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvt_roundsd_u64 (m128d, 7); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvt_roundsd_i64 (m128d, 7); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvtt_roundss_u64 (m128, 7); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvtt_roundss_i64 (m128, 7); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvtt_roundsd_u64 (m128d, 7); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvtt_roundsd_i64 (m128d, 7); /* { dg-error "incorrect rounding operand" } */
}

void
test_round_sae_64 (void)
{
  m128d = _mm_cvt_roundu64_sd (m128d, 4, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_cvt_roundi64_sd (m128d, 4, 5); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_cvt_roundu64_ss (m128, 4, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundi64_ss (m128, 4, 5); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvt_roundss_u64 (m128, 5); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvt_roundss_i64 (m128, 5); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvt_roundsd_u64 (m128d, 5); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvt_roundsd_i64 (m128d, 5); /* { dg-error "incorrect rounding operand" } */
}

void
test_sae_only_64 (void)
{
  ul = _mm_cvtt_roundss_u64 (m128, 3); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvtt_roundss_i64 (m128, 3); /* { dg-error "incorrect rounding operand" } */

  ul = _mm_cvtt_roundsd_u64 (m128d, 3); /* { dg-error "incorrect rounding operand" } */
  l = _mm_cvtt_roundsd_i64 (m128d, 3); /* { dg-error "incorrect rounding operand" } */
}
