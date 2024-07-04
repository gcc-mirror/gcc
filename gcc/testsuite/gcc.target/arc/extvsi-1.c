/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=em" } */
struct S { int a : 5; };

int foo (struct S *p)
{
  return p->a;
}

/* { dg-final { scan-assembler "msk_s\\s+r0,r0,4" } } */
/* { dg-final { scan-assembler "xor\\s+r0,r0,16" } } */
/* { dg-final { scan-assembler "sub\\s+r0,r0,16" } } */
/* { dg-final { scan-assembler-not "add3\\s+r0,r2,r0" } } */
/* { dg-final { scan-assembler-not "sext_s\\s+r0,r0" } } */
/* { dg-final { scan-assembler-not "asr_s\\s+r0,r0" } } */
