/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -ftree-vectorize -mveclibabi=acml -ffast-math" } */

double x[256];

extern double sin(double);

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    x[i] = sin(x[i]);
}

/* { dg-final { scan-assembler "__vrd2_sin" } } */
