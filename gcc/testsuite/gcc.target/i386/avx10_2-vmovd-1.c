/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vmovd\t\[0-9\]+\\(%e\[bs\]p\\), %xmm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovss\t\[0-9\]+\\(%e\[bs\]p\\), %xmm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovd\t%xmm0, %xmm0" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovd\t%edi, %xmm0" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovd\t%xmm0, %xmm0" 4 { target { ! ia32 } } } } */


#include<immintrin.h>

typedef int v4si __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));

v4si
__attribute__((noipa, unused))
f1 (int a)
{
  return __extension__(v4si){a, 0, 0, 0};
}

v4sf
__attribute__((noipa, unused))
f2 (float a)
{
  return __extension__(v4sf){a, 0, 0, 0};
}

v4si
__attribute__((noipa, unused))
f3 (v4si a)
{
  return __extension__(v4si){a[0], 0, 0, 0};
}

v4sf
__attribute__((noipa, unused))
f4 (v4sf a)
{
  return __extension__(v4sf){a[0], 0, 0, 0};
}

__m128i
__attribute__((noipa, unused))
f5 (__m128i a)
{
  return _mm_set_epi32 (0, 0, 0,((__v4si)a)[0]);
}
