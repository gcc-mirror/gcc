/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-options "-O" }  */
/* { dg-add-options arm_arch_v6 } */

unsigned short foo (unsigned short x)
{
  x ^= 0x4002;
  x >>= 1;
  x |= 0x8000;
  return x;
}

/* { dg-final { scan-assembler "eor" } } */
/* { dg-final { scan-assembler-not "mvn" } } */
/* { dg-final { scan-assembler-not "uxth" } } */
