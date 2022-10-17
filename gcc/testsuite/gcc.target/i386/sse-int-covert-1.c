/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mfpmath=sse -mtune-ctrl=^sse_partial_reg_converts_dependency" } */

extern float f;
extern int i;

void
foo (void)
{
  f = i;
}

/* { dg-final { scan-assembler "cvtsi2ssl" } } */
/* { dg-final { scan-assembler-not "pxor" } } */
