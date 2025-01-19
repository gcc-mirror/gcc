/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m0plus_small_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_cpu_cortex_m0plus_small } */

int
test (int a)
{
  return a * 0x123456;
}

/* { dg-final { scan-assembler-not "\[\\t \]+mul" } } */
