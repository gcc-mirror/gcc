/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2 -mfpmath=sse -mtune=generic" } */

double a[256];
int b[256];
unsigned short c[256];

extern long lrint (double);

void foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    b[i] = lrint (a[i]);
}

void bar(void)
{
  int i;

  for (i=0; i<256; ++i)
    {
      b[i] = lrint (a[i]);
      c[i] += c[i];
    }
}

/* { dg-final { scan-assembler "cvtpd2dq" } } */
