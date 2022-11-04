/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef __bf16 v4bf __attribute__ ((vector_size (8)));
typedef __bf16 v2bf __attribute__ ((vector_size (4)));

v4bf
v4bf_abi_1 (v4bf a)
{
  return a;
}

v4bf
v4bf_abi_3 (v4bf a, v4bf b, v4bf c)
{
  return c;
}

/* { dg-final { scan-assembler-times "movq\[\\t \]*%mm2, %mm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movaps\[\\t \]*%xmm2, %xmm0" 1 { target { ! ia32 } } } } */

v4bf
v4bf_abi_4 (v4bf a, v4bf b, v4bf c, v4bf d)
{
  return d;
}

/* { dg-final { scan-assembler-times "movq\[\\t \]*4\\(%esp\\), %mm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movaps\[\\t \]*%xmm3, %xmm0" 1 { target { ! ia32 } } } } */

v2bf
v2bf_test (v2bf a, v2bf b, v2bf c, v2bf d)
{
  return b;
}

/* { dg-final { scan-assembler-times "movl\[\\t \]*8\\(%esp\\), %eax" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movaps\[\\t \]*%xmm1, %xmm0" 1 { target { ! ia32 } } } } */
