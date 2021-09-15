/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mfpmath=sse -mtune-ctrl=use_vector_fp_converts" } */

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
/* { dg-final { scan-assembler "vcvtsi2ssl" } } */
/* { dg-final { scan-assembler-not "vcvtss2sd" } } */
/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm\[0-9\]" 1 } } */
