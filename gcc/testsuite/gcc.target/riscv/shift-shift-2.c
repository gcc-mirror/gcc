/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O" } */

/* Test for lshrsi3_zero_extend_3+1 pattern that uses p2m1_shift_operand.  */
unsigned int
sub1 (unsigned int i)
{
  return (i << 1) >> 1;
}

unsigned int
sub2 (unsigned int i)
{
  return (i << 20) >> 20;
}

unsigned long
sub3 (unsigned long i)
{
  return (i << 1) >> 1;
}

unsigned long
sub4 (unsigned long i)
{
  return (i << 52) >> 52;
}
/* { dg-final { scan-assembler-times "slli" 4 } } */
/* { dg-final { scan-assembler-times "srli" 4 } } */
