/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse -mtune-ctrl=^sse_partial_reg_fp_converts_dependency" } */

extern float f;
extern double d;

void
foo (void)
{
  d = f;
}

/* { dg-final { scan-assembler "vcvtss2sd" } } */
/* { dg-final { scan-assembler-not "vcvtps2pd" } } */
/* { dg-final { scan-assembler-not "vxorps" } } */
