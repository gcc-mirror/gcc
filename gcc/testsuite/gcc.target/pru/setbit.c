/* setbit instruction generation */

/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned int
test_setbit (unsigned int val)
{
  /* { dg-final { scan-assembler "set\\tr14, r14, 31" } } */
  val |= (1u << 31);
  return val;
}

