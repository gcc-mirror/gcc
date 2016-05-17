/* PR target/70809.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffp-contract=off -mtune=xgene1" } */

/* Check that vector FMLS is not generated when contraction is disabled.  */

void
foo (float *__restrict__ __attribute__ ((aligned (16))) a,
     float *__restrict__ __attribute__ ((aligned (16))) x,
     float *__restrict__ __attribute__ ((aligned (16))) y,
     float *__restrict__ __attribute__ ((aligned (16))) z)
{
  unsigned i = 0;
  for (i = 0; i < 256; i++)
    a[i] = x[i] - (y[i] * z[i]);
}

/* { dg-final { scan-assembler-not "fmls\tv.*" } } */
