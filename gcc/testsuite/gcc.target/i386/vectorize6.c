/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -ftree-vectorize -mveclibabi=svml -ffast-math" } */
/* { dg-require-effective-target sse2 } */

double x[256];

extern double sin(double);

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    x[i] = sin(x[i]);
}

/* { dg-final { scan-assembler "vmldSin2" } } */
