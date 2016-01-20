/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O1 -mfp16-format=ieee -march=armv7-a -mfpu=neon -mthumb -mrestrict-it" } */

__fp16 h0 = -1.0;

void
f (__fp16 *p)
{
  h0 = 1.0;
}
