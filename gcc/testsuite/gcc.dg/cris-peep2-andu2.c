/* { dg-do compile { target cris-*-* } } */
/* { dg-final { scan-assembler "movu.w \\\$r10,\\\$r" } } */
/* { dg-final { scan-assembler "and.w 2047,\\\$r" } } */
/* { dg-final { scan-assembler-not "move.d \\\$r10,\\\$r" } } */
/* { dg-final { scan-assembler "movu.b \\\$r10,\\\$r" } } */
/* { dg-final { scan-assembler "and.b 95,\\\$r" } } */
/* { dg-final { scan-assembler "andq -2,\\\$r" } } */
/* { dg-options "-O2" } */

/* Test the "andu" peephole2 trivially, register operand.  */

unsigned int
and_peep2_hi (unsigned int y, unsigned int *x)
{
  *x = y & 0x7ff;
  return y;
}

unsigned int
and_peep2_qi (unsigned int y, unsigned int *x)
{
  *x = y & 0x5f;
  return y;
}


unsigned int
and_peep2_q (unsigned int y, unsigned int *x)
{
  *x = y & 0xfe;
  return y;
}
