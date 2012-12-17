/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

void
f (int a)
{
  long long b = a;
  asm volatile ("@ extended to %0" : : "w" (b));
}

/* { dg-final { scan-assembler "vdup.32" } } */
/* { dg-final { scan-assembler "vshr.s64" } } */
