/* { dg-do compile } */
/* { dg-options "-O1 -funroll-loops -fvariable-expansion-in-unroller" } */

typedef int __m64 __attribute__ ((__vector_size__ (8)));
typedef int __v2si __attribute__ ((__vector_size__ (8)));

static __inline __m64 __attribute__((__always_inline__))
_mm_add_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddd ((__v2si)__m1, (__v2si)__m2);
}

__m64
simple_block_diff_up_mmx_4 (const int width, __m64 ref1)
{
  __m64 sum;
  int count = width >>1;
  while (count--)
    sum = _mm_add_pi32 (sum, ref1);
  return sum;
}
