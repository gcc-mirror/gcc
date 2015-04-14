/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=hard -mfpu=vfpv3-d16" } */

int foo (void)
{
  double x = 0.0;
  asm volatile ("" : "+gw" (x));
  return x;
}
