/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-final { scan-assembler-times "vmovw\t4\\(%esp\\), %xmm0" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovw\t8\\(%ebp\\), %xmm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovw\t%xmm0, %xmm0" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovw\t%edi, %xmm0" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovw\t%xmm0, %xmm0" 7 { target { ! ia32 } } } } */

#include<immintrin.h>

typedef _Float16 v8hf __attribute__((vector_size(16)));
typedef __bf16 v8bf __attribute__((vector_size(16)));
typedef short v8hi __attribute__((vector_size(16)));

v8hf
__attribute__((noipa, unused))
f1 (_Float16 a)
{
  return __extension__(v8hf){a, 0, 0, 0, 0, 0, 0, 0};
}

v8bf
__attribute__((noipa, unused))
f2 (__bf16 a)
{
  return __extension__(v8bf){a, 0, 0, 0, 0, 0, 0, 0};
}

v8hi
__attribute__((noipa, unused))
f3 (short a)
{
  return __extension__(v8hi){a, 0, 0, 0, 0, 0, 0, 0};
}

v8hf
__attribute__((noipa, unused))
f4 (v8hf a)
{
  return __extension__(v8hf){a[0], 0, 0, 0, 0, 0, 0, 0};
}

v8bf
__attribute__((noipa, unused))
f5 (v8bf a)
{
  return __extension__(v8bf){a[0], 0, 0, 0, 0, 0, 0, 0};
}

v8hi
__attribute__((noipa, unused))
f6 (v8hi a)
{
  return __extension__(v8hi){a[0], 0, 0, 0, 0, 0, 0, 0};
}

__m128i
__attribute__((noipa, unused))
f7 (__m128i a)
{
  return _mm_set_epi16 (0, 0, 0, 0, 0, 0, 0, ((__v8hi)a)[0]);
}

__m256h
__attribute__((noipa, unused))
f8 (_Float16 a)
{
  return _mm256_set_ph (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, a);
}
