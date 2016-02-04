/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mfma4 -ftree-vectorize -mtune=generic -mno-fma" } */

float r[256], s[256];
float x[256];
float y[256];
float z[256];

void foo (void)
{
  int i;
  for (i = 0; i < 256; ++i)
    {
      r[i] = x[i] * y[i] - z[i];
      s[i] = x[i] * y[i] + z[i];
    }
}

/* { dg-final { scan-assembler "vfmaddps" } } */
/* { dg-final { scan-assembler "vfmsubps" } } */
