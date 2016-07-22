/* { dg-do compile }  */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_fp16_ieee } */

/* Test __fp16 arguments and return value in registers (hard-float).  */

void
swap (__fp16, __fp16);

__fp16
F (__fp16 a, __fp16 b, __fp16 c)
{
  swap (b, a);
  return c;
}

/* { dg-final { scan-assembler-times {vmov\tr[0-9]+, s[0-2]} 2 } }  */
/* { dg-final { scan-assembler-times {vmov.f32\ts1, s0} 1 } }  */
/* { dg-final { scan-assembler-times {vmov\ts0, r[0-9]+} 2 } }  */
