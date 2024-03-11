/* Verify that the inline instance has no extra DW_TAG_lexical_block between
   the DW_TAG_inlined_subroutine and the DW_TAG_variable for the local.  */
/* { dg-options "-O -gdwarf -dA" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler "DW_TAG_inlined_subroutine\[^\\(\]*\(\|\\(\[^\\)\]*\\)\)\[^\\(\]*\\(DIE \\(0x\[0-9a-f\]*\\) DW_TAG_formal_parameter\[^\\(\]*\\(DIE \\(0x\[0-9a-f\]*\\) DW_TAG_variable" } } */
/* { dg-final { scan-assembler-times "DW_TAG_inlined_subroutine" 2 } } */

static int foo (int i)
{
  volatile int j = i + 3;
  return j - 2;
}
int main()
{
  volatile int z = foo (-1);
  return z;
}
