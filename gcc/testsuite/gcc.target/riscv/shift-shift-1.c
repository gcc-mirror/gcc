/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32 -O" } */

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
/* { dg-final { scan-assembler-times "slli" 2 } } */
/* { dg-final { scan-assembler-times "srli" 2 } } */
