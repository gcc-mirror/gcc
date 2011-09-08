/* Ensure replicated constants don't make things worse.  */
/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
foo1 (int a)
{
  /* It might be tempting to use 0x01000100, but it wouldn't help. */
  return a + 0x01f001e0;
}

/* { dg-final { scan-assembler "add.*#32505856" } } */
/* { dg-final { scan-assembler "add.*#480" } } */

int
foo2 (int a)
{
  return a + 0x0f100e10;
}

/* { dg-final { scan-assembler "add.*#252706816" } } */
/* { dg-final { scan-assembler "add.*#3600" } } */
