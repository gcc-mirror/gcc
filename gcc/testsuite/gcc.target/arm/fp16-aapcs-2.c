/* { dg-do compile }  */
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-options "-mfloat-abi=softfp -O2" }  */
/* { dg-add-options arm_fp16_ieee } */
/* { dg-skip-if "incompatible float-abi" { arm*-*-* } { "-mfloat-abi=hard" } } */

/* Test __fp16 arguments and return value in registers (softfp).  */

void
swap (__fp16, __fp16);

__fp16
F (__fp16 a, __fp16 b, __fp16 c)
{
  swap (b, a);
  return c;
}

/* { dg-final { scan-assembler-times {mov\tr[0-9]+, r[0-2]} 3 } }  */
/* { dg-final { scan-assembler-times {mov\tr1, r0} 1 } }  */
/* { dg-final { scan-assembler-times {mov\tr0, r[0-9]+} 2 } }  */
