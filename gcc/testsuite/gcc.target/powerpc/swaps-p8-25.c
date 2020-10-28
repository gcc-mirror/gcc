/* { dg-do compile { target le } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 " } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* Verify that swap optimization works correctly for a truncating splat.  */

/* Test case to resolve PR77613.  */

void pr77613 (signed short a, signed short *x, signed short *y)
{
  unsigned long i;

  for (i = 0; i < 1024; i++)
    y[i] = a * x[i] + y[i];
}
