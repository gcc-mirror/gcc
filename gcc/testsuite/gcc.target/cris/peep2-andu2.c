/* { dg-do assemble } */
/* { dg-final { scan-assembler "movu.w \\\$r10,\\\$|movu.w 2047," } } */
/* { dg-final { scan-assembler "and.w 2047,\\\$|and.d \\\$r10," } } */
/* { dg-final { scan-assembler-not "move.d \\\$r10,\\\$" } } */
/* { dg-final { scan-assembler "movu.b \\\$r10,\\\$|movu.b 95," } } */
/* { dg-final { scan-assembler "and.b 95,\\\$|and.d \\\$r10," } } */
/* { dg-final { scan-assembler "andq -2,\\\$" } } */
/* { dg-final { scan-assembler-not "movu.b 254,\\\$" } } */
/* { dg-options "-O2 -save-temps" } */

/* Originally used to test the "andu" peephole2 trivially, register operand.
   Due to reload changes (r186861), the suboptimal sequence isn't
   generated and the peephole2 doesn't trig for this trivial code
   anymore.  Another minimal sequence is generated, where the constant
   is loaded to a free register first.  Instead another case is exposed;
   handled by the "andqu" peephole2, trigged by and_peep2_q (the andq
   and scan-assembler-not-movu.b lines above).  */

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
