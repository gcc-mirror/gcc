/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-additional-options "-mcpu=cortex-m55+nodsp+nofp -mthumb -mfloat-abi=softfp -mfpu=auto --save-temps" } */
/* { dg-final { scan-assembler-not "\.arch_extension fp" } } */
/* { dg-final { scan-assembler-not "\.arch_extension fp.dp" } } */
/* { dg-final { scan-assembler-not "\.arch_extension dsp" } } */
/* { dg-final { scan-assembler-not "\.arch_extension mve" } } */
/* { dg-final { scan-assembler-not "\.arch_extension mve.fp" } } */
/* { dg-final { scan-assembler "\.fpu softvfp" } } */

int
f ()
{
  return 1;
}
