/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2 -fno-trapping-math" } */
/* { dg-final { scan-assembler-times "vcomisbf16\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6 } } */
/* { dg-final { scan-assembler-times {j[a-z]+\s} 6 } } */

__bf16
foo_eq (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a == b ? c + d : c - d;
}

__bf16
foo_ne (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a != b ? c + d : c - d;
}

__bf16
foo_lt (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a < b ? c + d : c - d;
}

__bf16
foo_le (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a <= b ? c + d : c - d;
}

__bf16
foo_gt (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a > b ? c + d : c - d;
}

__bf16
foo_ge (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a >= b ? c + d : c - d;
}
