/* Check that we can use the save instruction to save $16, $17 and $31.  */
/* { dg-mips-options "-mips32r2 -mgp32 -mips16 -O2" } */
void bar (void);
void
foo (void)
{
  bar ();
  asm volatile ("" ::: "$16", "$17");
}
/* { dg-final { scan-assembler "\tsave\t\[0-9\]*,\\\$16,\\\$17,\\\$31" } } */
/* { dg-final { scan-assembler "\trestore\t\[0-9\]*,\\\$16,\\\$17,\\\$31" } } */
