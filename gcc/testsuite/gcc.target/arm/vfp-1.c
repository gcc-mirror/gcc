/* { dg-do compile } */
/* { dg-options "-O2 -mfpu=vfp -mfloat-abi=softfp" } */
/* { dg-require-effective-target arm_vfp_ok } */

extern float fabsf (float);
extern float sqrtf (float);
extern double fabs (double);
extern double sqrt (double);

volatile float f1, f2, f3;

void test_sf() {
  /* abssf2_vfp */
  /* { dg-final { scan-assembler "fabss" } } */
  f1 = fabsf (f1);
  /* negsf2_vfp */
  /* { dg-final { scan-assembler "fnegs" } } */
  f1 = -f1;
  /* addsf3_vfp */
  /* { dg-final { scan-assembler "fadds" } } */
  f1 = f2 + f3;
  /* subsf3_vfp */
  /* { dg-final { scan-assembler "fsubs" } } */
  f1 = f2 - f3;
  /* divsf3_vfp */
  /* { dg-final { scan-assembler "fdivs" } } */
  f1 = f2 / f3;
  /* mulsf3_vfp */
  /* { dg-final { scan-assembler "fmuls" } } */
  f1 = f2 * f3;
  /* mulsf3negsf_vfp */
  /* { dg-final { scan-assembler "fnmuls" } } */
  f1 = -f2 * f3;
  /* mulsf3addsf_vfp */
  /* { dg-final { scan-assembler "fmacs" } } */
  f1 = f2 * f3 + f1;
  /* mulsf3subsf_vfp */
  /* { dg-final { scan-assembler "fmscs" } } */
  f1 = f2 * f3 - f1;
  /* mulsf3negsfaddsf_vfp */
  /* { dg-final { scan-assembler "fnmacs" } } */
  f1 = f2 - f3 * f1;
  /* mulsf3negsfsubsf_vfp */
  /* { dg-final { scan-assembler "fnmscs" } } */
  f1 = -f2 * f3 - f1;
  /* sqrtsf2_vfp */
  /* { dg-final { scan-assembler "fsqrts" } } */
  f1 = sqrtf (f1);
}

volatile double d1, d2, d3;

void test_df() {
  /* absdf2_vfp */
  /* { dg-final { scan-assembler "fabsd" } } */
  d1 = fabs (d1);
  /* negdf2_vfp */
  /* { dg-final { scan-assembler "fnegd" } } */
  d1 = -d1;
  /* adddf3_vfp */
  /* { dg-final { scan-assembler "faddd" } } */
  d1 = d2 + d3;
  /* subdf3_vfp */
  /* { dg-final { scan-assembler "fsubd" } } */
  d1 = d2 - d3;
  /* divdf3_vfp */
  /* { dg-final { scan-assembler "fdivd" } } */
  d1 = d2 / d3;
  /* muldf3_vfp */
  /* { dg-final { scan-assembler "fmuld" } } */
  d1 = d2 * d3;
  /* muldf3negdf_vfp */
  /* { dg-final { scan-assembler "fnmuld" } } */
  d1 = -d2 * d3;
  /* muldf3adddf_vfp */
  /* { dg-final { scan-assembler "fmacd" } } */
  d1 = d2 * d3 + d1;
  /* muldf3subdf_vfp */
  /* { dg-final { scan-assembler "fmscd" } } */
  d1 = d2 * d3 - d1;
  /* muldf3negdfadddf_vfp */
  /* { dg-final { scan-assembler "fnmacd" } } */
  d1 = d2 - d3 * d1;
  /* muldf3negdfsubdf_vfp */
  /* { dg-final { scan-assembler "fnmscd" } } */
  d1 = -d2 * d3 - d1;
  /* sqrtdf2_vfp */
  /* { dg-final { scan-assembler "fsqrtd" } } */
  d1 = sqrt (d1);
}

volatile int i1;
volatile unsigned int u1;

void test_convert () {
  /* extendsfdf2_vfp */
  /* { dg-final { scan-assembler "fcvtds" } } */
  d1 = f1;
  /* truncdfsf2_vfp */
  /* { dg-final { scan-assembler "fcvtsd" } } */
  f1 = d1;
  /* truncsisf2_vfp */
  /* { dg-final { scan-assembler "ftosizs" } } */
  i1 = f1;
  /* truncsidf2_vfp */
  /* { dg-final { scan-assembler "ftosizd" } } */
  i1 = d1;
  /* fixuns_truncsfsi2 */
  /* { dg-final { scan-assembler "ftouizs" } } */
  u1 = f1;
  /* fixuns_truncdfsi2 */
  /* { dg-final { scan-assembler "ftouizd" } } */
  u1 = d1;
  /* floatsisf2_vfp */
  /* { dg-final { scan-assembler "fsitos" } } */
  f1 = i1;
  /* floatsidf2_vfp */
  /* { dg-final { scan-assembler "fsitod" } } */
  d1 = i1;
  /* floatunssisf2 */
  /* { dg-final { scan-assembler "fuitos" } } */
  f1 = u1;
  /* floatunssidf2 */
  /* { dg-final { scan-assembler "fuitod" } } */
  d1 = u1;
}

void test_ldst (float f[], double d[]) {
  /* { dg-final { scan-assembler "flds.+ \\\[r0, #1020\\\]" } } */
  /* { dg-final { scan-assembler "flds.+ \\\[r\[0-9\], #-1020\\\]" { target { arm32 && { ! arm_thumb2_ok } } } } } */
  /* { dg-final { scan-assembler "add.+ r0, #1024" } } */
  /* { dg-final { scan-assembler "fsts.+ \\\[r\[0-9\], #0\\\]\n" } } */
  f[256] = f[255] + f[-255];

  /* { dg-final { scan-assembler "fldd.+ \\\[r1, #1016\\\]" } } */
  /* { dg-final { scan-assembler "fldd.+ \\\[r\[1-9\], #-1016\\\]" { target { arm32 && { ! arm_thumb2_ok } } } } } */
  /* { dg-final { scan-assembler "fstd.+ \\\[r1, #256\\\]" } } */
  d[32] = d[127] + d[-127];
}
