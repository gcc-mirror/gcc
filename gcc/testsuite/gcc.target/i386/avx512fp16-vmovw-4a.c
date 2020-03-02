/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

typedef short __v32hi __attribute__ ((__vector_size__ (64)));
typedef long long __m512i __attribute__ ((__vector_size__ (64), __may_alias__));

__m512i
__attribute__ ((noinline, noclone))
foo1 (short x)
{
  return __extension__ (__m512i)(__v32hi) { x, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0 };
}

__m512i
__attribute__ ((noinline, noclone))
foo2 (short *x)
{
  return __extension__ (__m512i)(__v32hi) { *x, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0,
					     0, 0, 0, 0, 0, 0, 0, 0 };
}

/* { dg-final { scan-assembler-times "vmovw\[^-\n\r]*xmm0" 2 } } */
