/* PR target/128229 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512dq -ftrapping-math -fdump-rtl-loop2_invariant" } */
/* These UNSPECs operate on float vectors but never raise floating point
   exceptions (SDM: "SIMD Floating-Point Exceptions: None"), so
   ix86_unspec_may_trap_p must leave them alone and the loop invariant
   pass must still be able to hoist them out of the guarded loop.  */

#include <immintrin.h>

/* UNSPEC_VPERMIL.  */
void
permil (__m256 a, __m256i idx, int *p, int *e, __m256 *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_permutevar_ps (a, idx);
}

/* UNSPEC_VPERMT2.  */
void
permt2 (__m256 a, __m256i idx, __m256 b, int *p, int *e, __m256 *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_permutex2var_ps (a, idx, b);
}

/* UNSPEC_RCP14.  */
void
rcp14 (__m256 a, int *p, int *e, __m256 *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_rcp14_ps (a);
}

/* UNSPEC_FPCLASS.  */
void
fpclass (__m256 a, int *p, int *e, __mmask8 *out)
{
  for (; p != e; p++, out++)
    if (*p)
      *out = _mm256_fpclass_ps_mask (a, 0x81);
}

/* { dg-final { scan-rtl-dump-times "Decided to move invariant" 4 "loop2_invariant" } } */
