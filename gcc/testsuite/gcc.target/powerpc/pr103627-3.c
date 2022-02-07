/* There are no error messages for either LE or BE 64bit.  */
/* { dg-require-effective-target be } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mdejagnu-cpu=power6" } */

/* Verify compiler emits error message instead of ICE.  */

/* Option -mno-avoid-indexed-addresses is to disable the unexpected
   warning on indexed addressing which can affect dg checks.  */
#pragma GCC target "cpu=power10,no-avoid-indexed-addresses"
int
main ()
{
  float *b;
  __vector_quad c;
  __builtin_mma_disassemble_acc (b, &c);
  /* { dg-error "'__builtin_mma_disassemble_acc' requires the '-mmma' option" "" { target *-*-* } .-1 } */
  return 0;
}

