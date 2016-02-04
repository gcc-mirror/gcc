/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-options "-O2 -ffp-contract=off" } */
/* { dg-add-options arm_fp } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */

extern float fabsf (float);
extern float sqrtf (float);
extern double fabs (double);
extern double sqrt (double);

volatile float f1, f2, f3;

void test_sf() {
  /* abssf2_vfp */
  /* { dg-final { scan-assembler "vabs.f32" } } */
  f1 = fabsf (f1);
  /* negsf2_vfp */
  /* { dg-final { scan-assembler "vneg.f32" } } */
  f1 = -f1;
  /* addsf3_vfp */
  /* { dg-final { scan-assembler "vadd.f32" } } */
  f1 = f2 + f3;
  /* subsf3_vfp */
  /* { dg-final { scan-assembler "vsub.f32" } } */
  f1 = f2 - f3;
  /* divsf3_vfp */
  /* { dg-final { scan-assembler "vdiv.f32" } } */
  f1 = f2 / f3;
  /* mulsf3_vfp */
  /* { dg-final { scan-assembler "vmul.f32" } } */
  f1 = f2 * f3;
  /* mulsf3negsf_vfp */
  /* { dg-final { scan-assembler "vnmul.f32" } } */
  f1 = -f2 * f3;
  /* mulsf3addsf_vfp */
  /* { dg-final { scan-assembler "vmla.f32" } } */
  f1 = f2 * f3 + f1;
  /* mulsf3subsf_vfp */
  /* { dg-final { scan-assembler "vnmls.f32" } } */
  f1 = f2 * f3 - f1;
  /* mulsf3negsfaddsf_vfp */
  /* { dg-final { scan-assembler "vmls.f32" } } */
  f1 = f2 - f3 * f1;
  /* mulsf3negsfsubsf_vfp */
  /* { dg-final { scan-assembler "vnmla.f32" } } */
  f1 = -f2 * f3 - f1;
  /* sqrtsf2_vfp */
  /* { dg-final { scan-assembler "vsqrt.f32" } } */
  f1 = sqrtf (f1);
}

volatile double d1, d2, d3;

void test_df() {
  /* absdf2_vfp */
  /* { dg-final { scan-assembler "vabs.f64" } } */
  d1 = fabs (d1);
  /* negdf2_vfp */
  /* { dg-final { scan-assembler "vneg.f64" } } */
  d1 = -d1;
  /* adddf3_vfp */
  /* { dg-final { scan-assembler "vadd.f64" } } */
  d1 = d2 + d3;
  /* subdf3_vfp */
  /* { dg-final { scan-assembler "vsub.f64" } } */
  d1 = d2 - d3;
  /* divdf3_vfp */
  /* { dg-final { scan-assembler "vdiv.f64" } } */
  d1 = d2 / d3;
  /* muldf3_vfp */
  /* { dg-final { scan-assembler "vmul.f64" } } */
  d1 = d2 * d3;
  /* muldf3negdf_vfp */
  /* { dg-final { scan-assembler "vnmul.f64" } } */
  d1 = -d2 * d3;
  /* muldf3adddf_vfp */
  /* { dg-final { scan-assembler "vmla.f64" } } */
  d1 = d2 * d3 + d1;
  /* muldf3subdf_vfp */
  /* { dg-final { scan-assembler "vnmls.f64" } } */
  d1 = d2 * d3 - d1;
  /* muldf3negdfadddf_vfp */
  /* { dg-final { scan-assembler "vmls.f64" } } */
  d1 = d2 - d3 * d1;
  /* muldf3negdfsubdf_vfp */
  /* { dg-final { scan-assembler "vnmla.f64" } } */
  d1 = -d2 * d3 - d1;
  /* sqrtdf2_vfp */
  /* { dg-final { scan-assembler "vsqrt.f64" } } */
  d1 = sqrt (d1);
}

volatile int i1;
volatile unsigned int u1;

void test_convert () {
  /* extendsfdf2_vfp */
  /* { dg-final { scan-assembler "vcvt.f64.f32" } } */
  d1 = f1;
  /* truncdfsf2_vfp */
  /* { dg-final { scan-assembler "vcvt.f32.f64" } } */
  f1 = d1;
  /* truncsisf2_vfp */
  /* { dg-final { scan-assembler "vcvt.s32.f32" } } */
  i1 = f1;
  /* truncsidf2_vfp */
  /* { dg-final { scan-assembler "vcvt.s32.f64" } } */
  i1 = d1;
  /* fixuns_truncsfsi2 */
  /* { dg-final { scan-assembler "vcvt.u32.f32" } } */
  u1 = f1;
  /* fixuns_truncdfsi2 */
  /* { dg-final { scan-assembler "vcvt.u32.f64" } } */
  u1 = d1;
  /* floatsisf2_vfp */
  /* { dg-final { scan-assembler "vcvt.f32.s32" } } */
  f1 = i1;
  /* floatsidf2_vfp */
  /* { dg-final { scan-assembler "vcvt.f64.s32" } } */
  d1 = i1;
  /* floatunssisf2 */
  /* { dg-final { scan-assembler "vcvt.f32.u32" } } */
  f1 = u1;
  /* floatunssidf2 */
  /* { dg-final { scan-assembler "vcvt.f64.u32" } } */
  d1 = u1;
}

void test_ldst (float f[], double d[]) {
  /* { dg-final { scan-assembler "vldr.32.+ \\\[r0, #-?\[0-9\]+\\\]" } } */
  /* { dg-final { scan-assembler "vldr.32.+ \\\[r\[0-9\], #-1020\\\]" { target { arm32 && { ! arm_thumb2_ok } } } } } */
  /* { dg-final { scan-assembler "add.+ r0, #1024" } } */
  /* { dg-final { scan-assembler "vstr.32.+ \\\[r\[0-9\]\\\]\n" } } */
  f[256] = f[255] + f[-255];

  /* { dg-final { scan-assembler "vldr.64.+ \\\[r1, #1016\\\]" } } */
  /* { dg-final { scan-assembler "vldr.64.+ \\\[r\[1-9\], #-1016\\\]" { target { arm32 && { ! arm_thumb2_ok } } } } } */
  /* { dg-final { scan-assembler "vstr.64.+ \\\[r1, #256\\\]" } } */
  d[32] = d[127] + d[-127];
}
