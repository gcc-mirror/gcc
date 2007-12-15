/* { dg-do assemble } */
/* { dg-final { scan-assembler "movu.w \\\$r10,\\\$" } } */
/* { dg-final { scan-assembler "and.w 2047,\\\$" } } */
/* { dg-final { scan-assembler-not "move.d \\\$r10,\\\$" } } */
/* { dg-final { scan-assembler "movu.b \\\$r10,\\\$" } } */
/* { dg-final { scan-assembler "and.b 95,\\\$" } } */
/* { dg-final { scan-assembler "andq -2,\\\$" } } */
/* { dg-options "-O2 -save-temps" } */

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
