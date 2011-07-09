/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -ftree-vectorize -mveclibabi=acml -ffast-math -mtune=generic" } */

double x[256];

extern double sin(double);

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    x[i] = sin(x[i]);
}

/* { dg-final { scan-assembler "__vrd2_sin" } } */
