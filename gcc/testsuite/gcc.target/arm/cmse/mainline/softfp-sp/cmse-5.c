/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* { dg-add-options arm_arch_v8m_main } */
/* { dg-skip-if "Do not combine float-abi= hard | soft | softfp" {*-*-*} {"-mfloat-abi=soft" -mfloat-abi=hard } {""} } */
/* { dg-skip-if "Skip these if testing double precision" {*-*-*} {"-mfpu=fpv[4-5]-d16"} {""} } */
/* { dg-options "-mcmse -mfloat-abi=softfp -mfpu=fpv5-sp-d16" }  */

extern float bar (void);

float __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return bar ();
}
/* { dg-final { scan-assembler "__acle_se_foo:" } } */
/* { dg-final { scan-assembler-not "mov\tr0, lr" } } */
/* { dg-final { scan-assembler "mov\tr1, lr" } } */
/* { dg-final { scan-assembler "mov\tr2, lr" } } */
/* { dg-final { scan-assembler "mov\tr3, lr" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts0, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts2, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts3, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts4, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts5, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts6, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts7, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts8, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts9, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts10, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts11, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts12, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts13, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts14, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts15, #1\.0" } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq, lr" { target { arm_arch_v8m_main_ok && { ! arm_dsp } } } } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvqg, lr" { target { arm_arch_v8m_main_ok && arm_dsp } } } } */
/* { dg-final { scan-assembler "push\t{r4}" } } */
/* { dg-final { scan-assembler "vmrs\tip, fpscr" } } */
/* { dg-final { scan-assembler "movw\tr4, #65376" } } */
/* { dg-final { scan-assembler "movt\tr4, #4095" } } */
/* { dg-final { scan-assembler "and\tip, r4" } } */
/* { dg-final { scan-assembler "vmsr\tfpscr, ip" } } */
/* { dg-final { scan-assembler "pop\t{r4}" } } */
/* { dg-final { scan-assembler "mov\tip, lr" } } */
/* { dg-final { scan-assembler "bxns" } } */
