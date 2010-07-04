/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2 -mfpmath=sse" } */

float a[256];
int b[256];
unsigned short c[256];

extern long lrintf (float);

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    b[i] = lrintf (a[i]);
}

void bar(void)
{
  int i;

  for (i=0; i<256; ++i)
    {
      b[i] = lrintf (a[i]);
      c[i] += c[i];
    }
}

/* { dg-final { scan-assembler "cvtps2dq" } } */
