/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */

/* { dg-final { scan-assembler-times "vpbroadcastw" 1 { target { ! ia32 } } } }  */
/* { dg-final { scan-assembler-times "vpblendw" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovsh" 1 { target { ! ia32 } } } }  */

/* { dg-final { scan-assembler-times "vpinsrw" 2 { target ia32 } } }  */
#include <immintrin.h>

typedef __bf16 __v8bf __attribute__ ((__vector_size__ (16)));
typedef __bf16 __m128bf16 __attribute__ ((__vector_size__ (16), __may_alias__));

__m128bf16
__attribute__ ((noinline, noclone))
foo1 (__m128bf16 a, __bf16 f)
{
  __v8bf x = (__v8bf) a;
  x[2] = f;
  return (__m128bf16) x;
}

__m128bf16
__attribute__ ((noinline, noclone))
foo2 (__m128bf16 a, __bf16 f)
{
  __v8bf x = (__v8bf) a;
  x[0] = f;
  return (__m128bf16) x;
}
