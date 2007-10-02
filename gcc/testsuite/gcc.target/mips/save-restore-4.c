/* Check that we can use the save instruction to save $16, $17 and $31.  */
/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-mips32r2 -mabi=32 -O2" } */
/* { dg-add-options mips16_attribute } */

void bar (void);

MIPS16 void
foo (void)
{
  bar ();
  asm volatile ("" ::: "$16", "$17");
}
/* { dg-final { scan-assembler "\tsave\t\[0-9\]*,\\\$16,\\\$17,\\\$31" } } */
/* { dg-final { scan-assembler "\trestore\t\[0-9\]*,\\\$16,\\\$17,\\\$31" } } */
