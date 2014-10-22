/* { dg-do compile } */
/* { dg-options "-O2 -fno-toplevel-reorder -mtune=bdver2" } */
/* { dg-additional-options "-mregparm=1 -msse -mfpmath=sse" { target ia32 } } */

void __attribute__ ((hot))
f1 (int x)
{
  register float f asm ("%xmm0") = x;
  asm volatile ("" :: "x" (f));
}

void __attribute__ ((cold))
f2 (int x)
{
  register float f asm ("%xmm1") = x;
  asm volatile ("" :: "x" (f));
}

void __attribute__ ((hot))
f3 (int x)
{
  register float f asm ("%xmm2") = x;
  asm volatile ("" :: "x" (f));
}

void __attribute__ ((cold))
f4 (int x)
{
  register float f asm ("%xmm3") = x;
  asm volatile ("" :: "x" (f));
}

/* { dg-final { scan-assembler "sp\\\), %xmm0" } } */
/* { dg-final { scan-assembler "(ax|di), %xmm1" } } */
/* { dg-final { scan-assembler "sp\\\), %xmm2" } } */
/* { dg-final { scan-assembler "(ax|di), %xmm3" } } */
