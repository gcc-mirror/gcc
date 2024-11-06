/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vmulnepbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vrcppbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */

typedef __bf16 v4bf __attribute__ ((__vector_size__ (8)));
typedef __bf16 v2bf __attribute__ ((__vector_size__ (4)));


__attribute__((optimize("fast-math")))
v4bf
foo_div_fast_math_4 (v4bf a, v4bf b)
{
  return a / b;
}

__attribute__((optimize("fast-math")))
v2bf
foo_div_fast_math_2 (v2bf a, v2bf b)
{
  return a / b;
}
