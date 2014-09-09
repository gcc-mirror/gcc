/* { dg-do compile } */
/* { dg-skip-if "no support for hard-float VFP ABI" { arm_thumb1 } { "-march=*" } { "" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "avoid conflicting -mfpu" { *-*-* } { "-mfpu=*" } { "-mfpu=fpv4-sp-d16" "-mfpu=vfpv3xd" "-mfpu=vfpv3xd-fp16" } } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=hard -mfpu=fpv4-sp-d16" }  */

int func1 (double d)
{
  return (int)d;
}
unsigned int func2 (double d)
{
  return (unsigned int)d;
}

/* { dg-final { scan-assembler-times "vmov\[\\t \]+r0,\[\\t \]*r1,\[\\t \]*d0" 2 { target { arm_little_endian } } } } */
/* { dg-final { scan-assembler-times "vmov\[\\t \]+r1,\[\\t \]*r0,\[\\t \]*d0" 2 { target { ! arm_little_endian } } } } */
