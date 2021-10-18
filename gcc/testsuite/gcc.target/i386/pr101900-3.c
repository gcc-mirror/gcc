/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse -mtune-ctrl=use_vector_fp_converts,use_vector_converts" } */

extern float f;
extern double d;
extern int i;

void
foo (void)
{
  d = f;
  f = i;
}

/* { dg-final { scan-assembler "vcvtps2pd" } } */
/* { dg-final { scan-assembler "vcvtdq2ps" } } */
/* { dg-final { scan-assembler-not "vcvtss2sd" } } */
/* { dg-final { scan-assembler-not "vcvtsi2ssl" } } */
/* { dg-final { scan-assembler-not "vxorps" } } */
