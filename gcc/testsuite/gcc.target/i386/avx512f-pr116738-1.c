/* PR target/116738 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "__builtin_ia32_min" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_ia32_max" "optimized" } } */

#include <x86intrin.h>

void
test_pr116738 (void)
{
  __m512 a = _mm512_setr_ps (1.f, 2.f, 0.f, -0.f, -0.f, 0.f, 5.f, 6.f, 7.f,
			     8.f, 9.f, 10.f, 11.f, -__builtin_inff (),
			     __builtin_inff (), -42.f);
  __m512 b = _mm512_setr_ps (-0.f, 3.f, -0.f, 0.f, -0.f, 0.f, 5.f, 5.f, 8.f,
			     7.f, 10.f, -9.f, 12.f, 0.f, -0.f, 42.f);
  __m512 w = _mm512_setr_ps (4.f, 5.f, 6.f, 7.f, 8.f, 9.f, 10.f, 0.f, 1.f,
			     2.f, 3.f, 4.f, 5.f, 6.f, 7.f, 8.f);
  __m512 c = _mm512_mask_min_ps (w, -1, a, b);
  __m512 d = _mm512_mask_min_ps (w, 18658, a, b);
  __m512 e = _mm512_mask_min_ps (w, 54649, a, b);
  __m512 f = _mm512_mask_max_ps (w, -1, a, b);
  __m512 g = _mm512_mask_max_ps (w, 18658, a, b);
  __m512 h = _mm512_mask_max_ps (w, 54649, a, b);
  __m128 i = _mm_setr_ps (1.f, 2.f, 0.f, -0.f);
  __m128 j = _mm_setr_ps (-0.f, 3.f, -0.f, 0.f);
  __m128 k = _mm_min_ss (i, j);
  __m128 l = _mm_max_ss (j, i);
  __m512 ce = _mm512_setr_ps (-0.f, 2.f, -0.f, 0.f, -0.f, 0.f, 5.f, 5.f, 7.f,
			      7.f, 9.f, -9.f, 11.f, -__builtin_inff (),
			      -0.f, -42.f);
  __m512 de = _mm512_setr_ps (4.f, 2.f, 6.f, 7.f, 8.f, 0.f, 5.f, 5.f, 1.f,
			      2.f, 3.f, -9.f, 5.f, 6.f, -0.f, 8.f); 
  __m512 ee = _mm512_setr_ps (-0.f, 5.f, 6.f, 0.f, -0.f, 0.f, 5.f, 0.f, 7.f,
			      2.f, 9.f, 4.f, 11.f, 6.f, -0.f, -42.f);
  __m512 fe = _mm512_setr_ps (1.f, 3.f, -0.f, 0.f, -0.f, 0.f, 5.f, 6.f, 8.f,
			      8.f, 10.f, 10.f, 12.f, 0.f, __builtin_inff (),
			      42.f);
  __m512 ge = _mm512_setr_ps (4.f, 3.f, 6.f, 7.f, 8.f, 0.f, 5.f, 6.f, 1.f,
			      2.f, 3.f, 10.f, 5.f, 6.f, __builtin_inff (),
			      8.f);
  __m512 he = _mm512_setr_ps (1.f, 5.f, 6.f, 0.f, -0.f, 0.f, 5.f, 0.f, 8.f,
			      2.f, 10.f, 4.f, 12.f, 6.f, __builtin_inff (),
			      42.f);
  __m128 ke = _mm_setr_ps (-0.f, 2.f, 0.f, -0.f);
  __m128 le = _mm_setr_ps (1.f, 3.f, -0.f, 0.f);
  if (__builtin_memcmp (&c, &ce, sizeof (c))
      || __builtin_memcmp (&d, &de, sizeof (d))
      || __builtin_memcmp (&e, &ee, sizeof (e))
      || __builtin_memcmp (&f, &fe, sizeof (f))
      || __builtin_memcmp (&g, &ge, sizeof (g))
      || __builtin_memcmp (&h, &he, sizeof (h))
      || __builtin_memcmp (&k, &ke, sizeof (k))
      || __builtin_memcmp (&l, &le, sizeof (l)))
    __builtin_abort ();
}
