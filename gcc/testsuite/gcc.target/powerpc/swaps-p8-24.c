/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -ffast-math" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* Verify that swap optimization works correctly in the presence of
   a V4SFmode reduction.  */

extern double optvalue;
extern void obfuscate (float, unsigned int);

void
foo (float *x, float *y, unsigned int n, unsigned int m)
{
  unsigned int i, j;
  float sacc;
  for (j = 0; j < m; ++j)
    {
      sacc = 0.0f;
      for (i = 0; i < n; ++i)
	sacc += x[i] * y[i];
      obfuscate (sacc, n);
    }
  optvalue = n * 2.0f * m;
}
