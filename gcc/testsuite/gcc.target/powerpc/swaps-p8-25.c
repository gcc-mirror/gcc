/* { dg-do compile { target le } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 " } */
/* { dg-require-effective-target powerpc_vsx } */
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
