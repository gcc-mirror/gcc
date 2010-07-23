/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

unsigned int a[256];
double b[256];

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    b[i] = a[i];
}

/* { dg-final { scan-assembler "cvtdq2pd" } } */

