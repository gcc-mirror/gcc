/* { dg-do compile }  */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-O2 -mfp16-format=alternative" }  */

/* Test __fp16 arguments and return value in registers (hard-float).  */

void
swap (__fp16, __fp16);

__fp16
F (__fp16 a, __fp16 b, __fp16 c)
{
  swap (b, a);
  return c;
}

/* { dg-final { scan-assembler {vmov\.f32\ts[0-9]+, s1} } }  */
/* { dg-final { scan-assembler {vmov\.f32\ts1, s0} } }  */
/* { dg-final { scan-assembler {vmov\.f32\ts[0-9]+, s2+} } }  */
/* { dg-final { scan-assembler-times {vmov\.f32\ts0, s[0-9]+} 2 } }  */

