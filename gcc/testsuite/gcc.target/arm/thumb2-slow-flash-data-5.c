/* { dg-do compile } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-m4" "-mcpu=cortex-m7" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-march=armv7e-m+fp -mfloat-abi=hard -O2 -mthumb -mslow-flash-data" } */

double __attribute__ ((target ("fpu=fpv5-sp-d16")))
foo (void)
{
  return 1.0f;
}

/* { dg-final { scan-assembler-not "#1\\.0e\\+0" } } */
