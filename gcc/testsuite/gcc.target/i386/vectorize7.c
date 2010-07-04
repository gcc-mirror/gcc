/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

unsigned int a[256];
float b[256];

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    b[i] = a[i];
}

/* { dg-final { scan-assembler "cvtdq2ps" } } */
