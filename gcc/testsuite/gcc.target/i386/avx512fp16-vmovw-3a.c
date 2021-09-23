/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef short __v16hi __attribute__ ((__vector_size__ (32)));
typedef long long __m256i __attribute__ ((__vector_size__ (32), __may_alias__));

__m256i
__attribute__ ((noinline, noclone))
foo1 (short x)
{
  return __extension__ (__m256i)(__v16hi) { x, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0 };
}

__m256i
__attribute__ ((noinline, noclone))
foo2 (short *x)
{
  return __extension__ (__m256i)(__v16hi) { *x, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0 };
}

/* { dg-final { scan-assembler-times "vmovw\[^-\n\r]*xmm0" 2 } } */
