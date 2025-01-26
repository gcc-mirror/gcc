/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[^\n\r\]*xmm\[0-9\]" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[^\n\r\]*xmm\[0-9\]" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[^\n\r\]*xmm\[0-9\]" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[^\n\r\]*xmm\[0-9\]" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[^\n\r\]*xmm\[0-9\]" 2 { target { ! ia32 } } } } */

typedef __bf16 v4bf __attribute__ ((__vector_size__ (8)));
typedef __bf16 v2bf __attribute__ ((__vector_size__ (4)));

v4bf
foo_madd_64 (v4bf a, v4bf b, v4bf c)
{
  return a * b + c;
}

v4bf
foo_msub_64 (v4bf a, v4bf b, v4bf c)
{
  return a * b - c;
}

v4bf
foo_nmadd_64 (v4bf a, v4bf b, v4bf c)
{
  return -a * b + c;
}

v4bf
foo_nmsub_64 (v4bf a, v4bf b, v4bf c)
{
  return -a * b - c;
}

v2bf
foo_madd_32 (v2bf a, v2bf b, v2bf c)
{
  return a * b + c;
}

v2bf
foo_msub_32 (v2bf a, v2bf b, v2bf c)
{
  return a * b - c;
}

v2bf
foo_nmadd_32 (v2bf a, v2bf b, v2bf c)
{
  return -a * b + c;
}

v2bf
foo_nmsub_32 (v2bf a, v2bf b, v2bf c)
{
  return -a * b - c;
}
