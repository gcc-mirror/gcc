/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_main_ok } */
/* { dg-add-options arm_arch_v8m_main } */
/* { dg-skip-if "Do not combine float-abi= hard | soft | softfp" {*-*-*} {"-mfloat-abi=hard" -mfloat-abi=softfp } {""} } */
/* { dg-options "-mcmse -mfloat-abi=soft" }  */

extern float bar (void);

float __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return bar ();
}

/* { dg-final { scan-assembler "mov\tr1, lr" } } */
/* { dg-final { scan-assembler "mov\tr2, lr" } } */
/* { dg-final { scan-assembler "mov\tr3, lr" } } */
/* { dg-final { scan-assembler "mov\tip, lr" } } */
/* { dg-final { scan-assembler-not "vmov" } } */
/* { dg-final { scan-assembler-not "vmsr" } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvq, lr" { target { arm_arch_v8m_main_ok && { ! arm_dsp } } } } } */
/* { dg-final { scan-assembler "msr\tAPSR_nzcvqg, lr" { target { arm_arch_v8m_main_ok && arm_dsp } } } } */
/* { dg-final { scan-assembler "bxns" } } */

