/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-skip-if "do not override fpu" { *-*-* } { "-mfpu=*" } { "-mfpu=fpv5-sp-d16" } } */
/* { dg-skip-if "do not disable fpu" { *-*-* } { "-mfloat-abi=soft" } { * } } */
/* { dg-skip-if "do not override fp16-format" { *-*-* } { "-mfp16-format=*" } { "-mfp16-format=ieee" } } */
/* { dg-options "-O1 -mfpu=fpv5-sp-d16 -mfloat-abi=hard -mfp16-format=ieee" } */

__fp16 foo (double a)
{
  return a;
}

double bar (__fp16 a)
{
  return a;
}
