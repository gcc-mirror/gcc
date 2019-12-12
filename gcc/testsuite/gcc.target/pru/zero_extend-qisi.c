/* ALU operations with zero extended operands. */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it the zero extension might not
   be coalesced into the ALU instruction. */

unsigned int
test_zext_plus_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "add\\tr14, r14.b0, r15" } } */
  return val1 + val2;
}

unsigned int
test_zext_minus_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "sub\\tr14, r14.b0, r15" } } */
  return val1 - val2;
}

unsigned int
test_zext_xor_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "xor\\tr14, r14.b0, r15" } } */
  return val1 ^ val2;
}

unsigned int
test_zext_or_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "or\\tr14, r14.b0, r15" } } */
  return val1 | val2;
}

unsigned int
test_zext_ashl_hi (unsigned char val1, unsigned int val2)
{
  /* { dg-final { scan-assembler "lsl\\tr14, r14.b0, r15" } } */
  return val1 << val2;
}

