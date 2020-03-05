/* PR target/93418 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

void link_error (void);

void
foo (void)
{
  __m128i a = _mm_set1_epi32 (0xffffffffU);
  __m128i b = _mm_setr_epi32 (16, 31, -34, 3);
  __m128i c = _mm_sllv_epi32 (a, b);
  __v4su d = (__v4su) c;
  if (d[0] != 0xffff0000U || d[1] != 0x80000000U
      || d[2] != 0 || d[3] != 0xfffffff8U)
    link_error ();
}
