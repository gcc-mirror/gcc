/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx" } */

void foo (int * __restrict a, int *b, int s)
{
  for (int i = 0; i < 1024; ++i)
    {
      a[8*i+0] = b[s*i+0];
      a[8*i+1] = b[s*i+1];
      a[8*i+2] = b[s*i+2];
      a[8*i+3] = b[s*i+3];
      a[8*i+4] = b[s*i+4];
      a[8*i+5] = b[s*i+5];
      a[8*i+6] = b[s*i+4];
      a[8*i+7] = b[s*i+5];
    }
}

/* Three two-element loads, two four-element stores.  On ia32 we elide
   a permute and perform a redundant load.  */
/* { dg-final { scan-assembler-times "movq" 2 } } */
/* { dg-final { scan-assembler-times "movhps" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movhps" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movups" 2 } } */
