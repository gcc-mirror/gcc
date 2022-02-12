/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* Test for lshrsi3_zero_extend_3+2 pattern that uses
   high_mask_shift_operand.  */
unsigned long
sub1 (unsigned long i)
{
  return (i >> 32) << 32;
}

unsigned long
sub2 (unsigned long i)
{
  return (i >> 63) << 63;
}
/* { dg-final { scan-assembler-times "slli" 2 } } */
/* { dg-final { scan-assembler-times "srli" 2 } } */
