/* { dg-do compile } */
/* { dg-options "-mcmse -mfloat-abi=hard -mfpu=fpv5-d16" }  */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* { dg-add-options arm_arch_v8m_main } */
/* { dg-skip-if "Do not combine float-abi= hard | soft | softfp" {*-*-*} {"-mfloat-abi=soft" -mfloat-abi=softfp } {""} } */
/* { dg-skip-if "Skip these if testing single precision" {*-*-*} {"-mfpu=*-sp-*"} {""} } */

extern float bar (void);

float __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return bar ();
}
/* { dg-final { scan-assembler "mov\tr0, lr" } } */
/* { dg-final { scan-assembler "mov\tr1, lr" } } */
/* { dg-final { scan-assembler "mov\tr2, lr" } } */
/* { dg-final { scan-assembler "mov\tr3, lr" } } */
/* { dg-final { scan-assembler-not "vmov\.f32\ts0, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f32\ts1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td1, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td2, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td3, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td4, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td5, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td6, #1\.0" } } */
/* { dg-final { scan-assembler "vmov\.f64\td7, #1\.0" } } */
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
