/* Verify that at least one of both inline instances have
   a DW_AT_ranges but no extra DW_TAG_lexical_block.  */
/* { dg-options "-Os -gdwarf -dA" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler-times "\\(DIE \\(\[^\n\]*\\) DW_TAG_inlined_subroutine" 2 } } */
/* { dg-final { scan-assembler " DW_AT_ranges" } } */
/* { dg-final { scan-assembler-times "\\(DIE \\(\[^\n\]*\\) DW_TAG_lexical_block" 0 } } */

static int foo (int i)
{
  volatile int j = i + 3;
  if (j == 3)
    return 0;
  return j - 2;
}
int main()
{
  volatile int z = foo (-2) && foo (-1);
  return z;
}
