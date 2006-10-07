/* Verify that the scheduler does not discard the lexical block.  */
/* { dg-do compile } */
/* { dg-options "-dA" } */
/* On MIPS targets that support conditional moves, the optimal
   implementation of this function is:

	l[wd] tmp,p
	li $2,2
	jr $31
	movz $2,$0,tmp

   After if-conversion, we have a conditional move into a pseudo P
   followed a copy of P into the return register ($2).  P is associated
   with xyzzy, so if-conversion is behaving as expected, and has not lost
   the variable association.  The destination of the second instruction
   is associated with the function return value.  Combine then combines
   these two instructions, removing the last use of P and xyzzy.

   Everything is behaving as expected in this scenario, so we avoid
   using conditional moves for this test.  */
/* { dg-options "-dA -fno-if-conversion" { target mips*-*-* } } */
/* { dg-final { scan-assembler "xyzzy" } } */

long p;

long foo(void)
{
  {
    long xyzzy = 0;
    if (p)
      xyzzy = 2;
    return xyzzy;
  }
}
