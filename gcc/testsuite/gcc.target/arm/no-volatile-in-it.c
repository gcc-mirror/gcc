/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m7_ok } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_cpu_cortex_m7 } */

int
foo (int a, int b, volatile int *c, volatile int *d)
{
  if (a > b)
    return c[0];
  else
    return d[0];
}

/* { dg-final { scan-assembler-not "ldrgt" } } */
