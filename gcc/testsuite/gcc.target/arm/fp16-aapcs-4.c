/* { dg-do compile }  */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfloat-abi=softfp -O2" }  */
/* { dg-add-options arm_fp16_alternative } */
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

/* The swap must include two moves out of r0/r1 and two moves in.  */
/* { dg-final { scan-assembler-times {mov\tr[0-9]+, r[01]} 2 } }  */
/* { dg-final { scan-assembler-times {mov\tr[01], r[0-9]+} 2 } }  */
/* c should be spilled around the call.  */
/* { dg-final { scan-assembler {str\tr2, ([^\n]*).*ldrh\tr0, \1} { target arm_little_endian } } } */
