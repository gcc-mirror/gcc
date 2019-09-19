/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -mfpmath=sse" } */

extern float f;
extern double d;
extern unsigned char c;

void
foo (int n, int k)
{
  for (int i = 0; i != n; i++)
    if(i < k)
      d = c;
    else
      f = c;
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
