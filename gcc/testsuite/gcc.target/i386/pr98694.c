/* PR rtl-optimization/98694 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#include<immintrin.h>
typedef short v4hi __attribute__ ((vector_size (8)));
typedef int v2si __attribute__ ((vector_size (8)));
v4hi b;

__attribute__ ((noipa))
v2si
foo (__m512i src1, __m512i src2)
{
  __mmask64 m = _mm512_cmpeq_epu8_mask (src1, src2);
  short s = (short) m;
  int i = (int)m;
  b = __extension__ (v4hi) {s, s, s, s};
  return __extension__ (v2si) {i, i};
}

int main ()
{
  if (!__builtin_cpu_supports ("avx512bw"))
    return 0;

  __m512i src1 = _mm512_setzero_si512 ();
  __m512i src2 = _mm512_set_epi8 (0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1,
				  0, 1, 0, 1, 0, 1, 0, 1);
  __mmask64 m = _mm512_cmpeq_epu8_mask (src1, src2);
  v2si a = foo (src1, src2);
  if (a[0] != (int)m)
    __builtin_abort ();
  return 0;
}
