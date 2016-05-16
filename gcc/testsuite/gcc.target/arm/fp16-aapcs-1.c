/* { dg-do compile }  */
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-options "-mfp16-format=ieee -O2" }  */
/* { dg-add-options arm_fp16 } */

/* Test __fp16 arguments and return value in registers.  */

__fp16 F (__fp16 a, __fp16 b, __fp16 c)
{
  if (a == b)
    return c;
  return a;
}

/* { dg-final { scan-assembler-times {vcvtb\.f32\.f16\ts[0-9]+, s0} 1 } }  */
/* { dg-final { scan-assembler-times {vcvtb\.f32\.f16\ts[0-9]+, s1} 1 } }  */
/* { dg-final { scan-assembler-times {vmov\ts0, r[0-9]+} 1 } }  */
