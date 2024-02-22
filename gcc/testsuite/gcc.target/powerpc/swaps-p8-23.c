/* { dg-do compile { target le } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 -ffast-math" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" { target le } } } */

/* Verify that swap optimization works correctly in the presence of
   a V2DFmode reduction.  */

extern double optvalue;
extern void obfuscate (double, unsigned int);

void
foo (double *x, double *y, unsigned int n, unsigned int m)
{
  unsigned int i, j;
  double sacc;
  for (j = 0; j < m; ++j)
    {
      sacc = 0.0;
      for (i = 0; i < n; ++i)
	sacc += x[i] * y[i];
      obfuscate (sacc, n);
    }
  optvalue = n * 2.0 * m;
}
