/* AND with zero extension of operands.
   It is matched slightly different than rest of ALU ops. */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it the zero extension might not
   be coalesced into the ALU instruction. */

unsigned int
test_zext_and_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "and\\tr14, r14.b0, r15" } } */
  return val1 & val2;
}

