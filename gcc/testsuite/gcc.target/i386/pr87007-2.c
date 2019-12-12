/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse" } */

extern float f;
extern double d;
extern int i;

void
foo (int n, int k)
{
  for (int i = 0; i != n; i++)
    if(i < k)
      d = f;
    else
      f = i;
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
