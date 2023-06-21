/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* Cope with --enable-frame-pointer.  */
/* { dg-additional-options "-fomit-frame-pointer" } */

typedef _Float16 v4hf __attribute__ ((vector_size (8)));
typedef _Float16 v2hf __attribute__ ((vector_size (4)));

v4hf
v4hf_abi_1 (v4hf a)
{
  return a;
}

v4hf
v4hf_abi_3 (v4hf a, v4hf b, v4hf c)
{
  return c;
}

/* { dg-final { scan-assembler-times "movq\[\\t \]*%mm2, %mm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovaps\[\\t \]*%xmm2, %xmm0" 1 { target { ! ia32 } } } } */

v4hf
v4hf_abi_4 (v4hf a, v4hf b, v4hf c, v4hf d)
{
  return d;
}

/* { dg-final { scan-assembler-times "movq\[\\t \]*4\\(%esp\\), %mm0" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovaps\[\\t \]*%xmm3, %xmm0" 1 { target { ! ia32 } } } } */

v2hf
v2hf_test (v2hf a, v2hf b, v2hf c, v2hf d)
{
  return b;
}

/* { dg-final { scan-assembler-times "movl\[\\t \]*8\\(%esp\\), %eax" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vmovaps\[\\t \]*%xmm1, %xmm0" 1 { target { ! ia32 } } } } */
