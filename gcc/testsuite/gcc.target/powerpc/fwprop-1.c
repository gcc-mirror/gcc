/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-fwprop1-details" } */
/* { dg-final { scan-rtl-dump-not "propagating insn" "fwprop1" } } */

/* Verify that volatile asm operands doesn't try to be propagated.  */
long long foo ()
{
  long long res;
  __asm__ __volatile__(
    ""
      : "=r" (res)
      :
      : "memory");
  return res;
}
