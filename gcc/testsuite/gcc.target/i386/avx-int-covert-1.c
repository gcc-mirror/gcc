/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse -mtune-ctrl=^sse_partial_reg_converts_dependency" } */

extern float f;
extern int i;

void
foo (void)
{
  f = i;
}

/* { dg-final { scan-assembler "vcvtsi2ssl" } } */
/* { dg-final { scan-assembler-not "vxorps" } } */
