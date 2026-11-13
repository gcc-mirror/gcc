/* PR target/128229 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx10.2 -ftrapping-math -fdump-rtl-loop2_invariant" } */
/* UNSPEC_GETEXP and UNSPEC_PCMP are shared with vgetexpbf16 and vcmpbf16,
   which never raise floating point exceptions.  Make sure the mode check
   in ix86_unspec_may_trap_p still lets them be hoisted out of the loop.  */

#include <immintrin.h>

void
getexp_bf16 (__m256bh a, int *p, int *e, __m256bh *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_getexp_pbh (a);
}

void
cmp_bf16 (__m256bh a, __m256bh b, int *p, int *e, __mmask16 *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_cmp_pbh_mask (a, b, _CMP_LT_OS);
}

/* { dg-final { scan-rtl-dump-times "Decided to move invariant" 2 "loop2_invariant" } } */
