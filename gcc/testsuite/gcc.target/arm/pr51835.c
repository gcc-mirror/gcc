/* { dg-do compile } */
/* { dg-options "-O2 -mfloat-abi=hard -mfpu=fpv4-sp-d16" }  */
/* { dg-require-effective-target arm_thumb2_ok } */

int func1 (double d)
{
  return (int)d;
}
unsigned int func2 (double d)
{
  return (unsigned int)d;
}

/* { dg-final { scan-assembler-times "fmrrd\[\\t \]+r0,\[\\t \]*r1,\[\\t \]*d0" 2 { target { arm_little_endian } } } } */
/* { dg-final { scan-assembler-times "fmrrd\[\\t \]+r1,\[\\t \]*r0,\[\\t \]*d0" 2 { target { ! arm_little_endian } } } } */
