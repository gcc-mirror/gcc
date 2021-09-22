/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse -mtune-ctrl=^sse_partial_reg_fp_converts_dependency,^sse_partial_reg_converts_dependency" } */

extern float f;
extern double d;
extern int i;

void
foo (void)
{
  d = f;
  f = i;
}

/* { dg-final { scan-assembler "vcvtss2sd" } } */
/* { dg-final { scan-assembler "vcvtsi2ssl" } } */
/* { dg-final { scan-assembler-not "vcvtps2pd" } } */
/* { dg-final { scan-assembler-not "vcvtdq2ps" } } */
/* { dg-final { scan-assembler-not "vxorps" } } */
