/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse" } */

extern float f;
extern double d;
extern int i;

void
foo (void)
{
  d = f;
  f = i;
}

/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
