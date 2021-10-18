/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));

__m128i
__attribute__ ((noinline, noclone))
foo1 (short x)
{
  return __extension__ (__m128i)(__v8hi) { x, 0, 0, 0, 0, 0, 0, 0 };
}

__m128i
__attribute__ ((noinline, noclone))
foo2 (short *x)
{
  return __extension__ (__m128i)(__v8hi) { *x, 0, 0, 0, 0, 0, 0, 0 };
}

/* { dg-final { scan-assembler-times "vmovw\[^-\n\r]*xmm0" 2  } } */
