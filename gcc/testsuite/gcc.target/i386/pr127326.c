/* PR rtl-optimization/127326 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target dfp } */

/* On 32-bit x86 no hard register can hold a TDmode value, so the "+g"
   asm operand has to be matched in memory.  LRA used to reject this with
   "inconsistent operand constraints in an 'asm'".  */

_Decimal128
test (void)
{
  union { _Decimal128 d; unsigned long long u[2]; } u;
  u.d = 1.0DL;
  __asm ("" : "+g" (u.d)); /* { dg-bogus "inconsistent operand constraints" } */
  return u.d;
}
