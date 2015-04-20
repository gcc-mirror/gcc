/* { dg-do compile } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16" } */

int foo (void)
{
  double x = 0.0;
  asm volatile ("" : "+gw" (x));
  return x;
}
