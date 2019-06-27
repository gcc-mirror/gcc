/* Test QBBC recognition */

/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it bit-check-and-branch
   operation may not be optimized to QBBC.  */

unsigned int
test_qbbc_reg (unsigned int a, unsigned int b, unsigned short val)
{
  /* { dg-final { scan-assembler "qbbc\\t.L\[0-9\]*, r16.w0, 12" } } */
  if (!(val & (1 << 12)))
    return a;
  return b;
}
