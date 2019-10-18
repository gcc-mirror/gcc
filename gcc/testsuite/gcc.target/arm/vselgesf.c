/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-require-effective-target arm_v8_vfp_ok } */
/* { dg-options "-O2 -mcpu=cortex-a57" } */
/* { dg-add-options arm_v8_vfp } */

float
foo (float x, float y)
{
  volatile int i = 0;
  return i >= 0 ? x : y;
}

/* { dg-final { scan-assembler-times "vselge.f32\ts\[0-9\]+" 1 } } */
