/* Check that GCC does bti instruction.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-add-options arm_arch_v8_1m_main } */
/* { dg-additional-options "-mfloat-abi=softfp -mbranch-protection=bti --save-temps" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler "\tbti" } } */
