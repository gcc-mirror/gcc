/* ALU operations with zero extended destination. */

/* { dg-do compile } */
/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it the zero extension might not
   be coalesced into the ALU instruction. */

unsigned int
test_zext_xor_hi (unsigned short val1, unsigned short val2)
{
  /* { dg-final { scan-assembler "xor\\tr14, r14.w0, r14.w2" } } */
  return (unsigned short)(val1 ^ val2);
}

unsigned int
test_zext_or_hi (unsigned short val1, unsigned short val2)
{
  /* { dg-final { scan-assembler "or\\tr14, r14.w0, r14.w2" } } */
  return (unsigned short)(val1 | val2);
}

unsigned int
test_zext_ashr_hi_const (unsigned short val1)
{
  /* { dg-final { scan-assembler "lsr\\tr14, r14.w0, 3" } } */
  return (unsigned short)(val1 >> 3);
}
