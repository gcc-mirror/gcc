/* clearbit instruction generation */

/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned int
test_clearbit (unsigned int val)
{
  /* { dg-final { scan-assembler "clr\\tr14, r14, 19" } } */
  val &= ~(1u << 19);
  return val;
}

