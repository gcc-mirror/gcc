/* Test LMBD builtin.  */

/* { dg-options "-O1" } */

/* -O1 in the options is significant.  Without it zero_extend
   operation may not be optimized.  */

unsigned int
test_lmbd (unsigned char a, unsigned short b)
{
  /* { dg-final { scan-assembler "lmbd\\tr14, r14.w1, r14.b0" } } */
  return __lmbd(b, a);
}

