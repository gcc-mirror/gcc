/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative" } */

float xx __attribute__((mode(HF))) = 0.0;

/* { dg-final { scan-assembler "\t.eabi_attribute 38, 2" } } */
/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.space\t2" } } */
