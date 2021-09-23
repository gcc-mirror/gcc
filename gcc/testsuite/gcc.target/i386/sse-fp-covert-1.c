/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mfpmath=sse -mtune-ctrl=^sse_partial_reg_fp_converts_dependency" } */

extern float f;
extern double d;

void
foo (void)
{
  d = f;
}

/* { dg-final { scan-assembler "cvtss2sd" } } */
/* { dg-final { scan-assembler-not "cvtps2pd" } } */
/* { dg-final { scan-assembler-not "pxor" } } */
