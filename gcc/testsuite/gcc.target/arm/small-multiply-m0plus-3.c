/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m0plus_small_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_cpu_cortex_m0plus_small } */

int
test (int a)
{
  return a * 0x13;
}

/* { dg-final { scan-assembler-not "\[\\t \]+mul" } } */
