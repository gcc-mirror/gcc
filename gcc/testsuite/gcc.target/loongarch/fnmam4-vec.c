/* { dg-do compile } */
/* { dg-options "-Ofast -mlasx -ftree-vectorize" } */
/* { dg-require-effective-target loongarch_asx } */

void
foo (float *u, float x, float *y, float z)
{
  int i;
  for (i = 0; i < 1024; i++)
    *(u++) = (x - y[i] * z);
}

/* { dg-final { scan-assembler-not "\tvori.b"} } */
/* { dg-final { scan-assembler-not "\txvori.b"} } */
