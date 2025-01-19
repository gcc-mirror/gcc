/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2" } */

/* { dg-final { scan-assembler-times "vcomsbf16\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6 } } */
/* { dg-final { scan-assembler-times "set\[aeglnb\]+" 6 } } */

#define AVX10_ATTR \
__attribute__((noinline, __target__("avx10.2"), optimize("no-trapping-math")))

AVX10_ATTR
int foo1_avx10 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a == b && c < d;
}

AVX10_ATTR
int foo2_avx10 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a > b || c != d;
}

AVX10_ATTR
int foo3_avx10 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return (a >= b) * (c <= d);
}

