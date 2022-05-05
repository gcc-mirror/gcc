/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* Test for <optab>si3_mask.  */
int
sub1 (int i, int j)
{
  return i << (j & 0x1f);
}
/* { dg-final { scan-assembler-not "andi" } } */
