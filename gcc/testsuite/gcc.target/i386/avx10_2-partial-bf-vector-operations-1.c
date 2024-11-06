/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vmulnepbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vaddnepbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vdivnepbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vsubnepbf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */

typedef __bf16 v4bf __attribute__ ((__vector_size__ (8)));
typedef __bf16 v2bf __attribute__ ((__vector_size__ (4)));

v4bf
foo_mul_4 (v4bf a, v4bf b)
{
  return a * b;
}

v4bf
foo_add_4 (v4bf a, v4bf b)
{
  return a + b;
}

v4bf
foo_div_4 (v4bf a, v4bf b)
{
  return a / b;
}

v4bf
foo_sub_4 (v4bf a, v4bf b)
{
  return a - b;
}

v2bf
foo_mul_2 (v2bf a, v2bf b)
{
  return a * b;
}

v2bf
foo_add_2 (v2bf a, v2bf b)
{
  return a + b;
}

v2bf
foo_div_2 (v2bf a, v2bf b)
{
  return a / b;
}

v2bf
foo_sub_2 (v2bf a, v2bf b)
{
  return a - b;
}
