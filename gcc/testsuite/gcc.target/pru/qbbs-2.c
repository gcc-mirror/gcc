/* Test QBBS recognition */

/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it bit-check-and-branch
   operation may not be optimized to QBBS.  */

unsigned int
test_qbbs_reg (unsigned int a, unsigned int b, unsigned int val)
{
  /* { dg-final { scan-assembler "qbbs\\t.L\[0-9\]*, r16, 19" } } */
  if (val & (1 << 19))
    return a;
  return b;
}
