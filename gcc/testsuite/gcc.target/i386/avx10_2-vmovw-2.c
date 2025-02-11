/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-require-effective-target avx10_2_256 } */

#define AVX10_2
#define AVX10_SCALAR

#include "avx10-helper.h"
#include "avx10_2-vmovw-1.c"

static void
TEST (void)
{
  union128i_w u1, s1;
  union256i_w u2, s2;
  short e1[8] = {0};
  short e2[16] = {0};

  s1.x = _mm_set_epi16(-12876, -12886, -12776, -22876, -22886, -22776, -32766, 30158);
  e1[0] = s1.a[0];

  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f1(((_Float16*)s1.a)[0]);
  if (check_union128i_w (u1, e1))
    abort ();

  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f2(((__bf16*)s1.a)[0]);
  if (check_union128i_w (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f3((short)s1.a[0]);
  if (check_union128i_w (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f4((v8hf)s1.x);
  if (check_union128i_w (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f5((v8bf)s1.x);
  if (check_union128i_w (u1, e1))
    abort ();
  
  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f6((v8hi)s1.x);
  if (check_union128i_w (u1, e1))
    abort ();

  u1.x = _mm_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1);
  u1.x = (__m128i)f7((__m128i)s1.x);
  if (check_union128i_w (u1, e1))
    abort ();

  s2.x = _mm256_set_epi16(-12876, -12886, -12776, -22876, -22886, -22776, -32766, 30158,
                          -12876, -12886, -12776, -22876, -22886, -22776, -32766, 30158);
  e2[0] = s2.a[0];  
  u2.x = _mm256_set_epi16(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);
  u2.x = (__m256i)f8(((_Float16*)s2.a)[0]);
  if (check_union256i_w (u2, e2))
    abort ();
}
