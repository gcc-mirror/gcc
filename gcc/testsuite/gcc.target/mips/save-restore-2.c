/* Check that we can use the save instruction to save spilled arguments.  */
/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-mips32r2 -mabi=32 -O2" } */
/* { dg-add-options mips16_attribute } */

MIPS16 void
foo (int *a, int b, int c)
{
  asm volatile ("" ::: "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		"$9", "$10", "$11", "$12", "$13", "$14", "$15", "$16",
		"$17", "$18", "$19", "$20", "$21", "$22", "$23", "$24",
		"$25", "$30", "memory");
  a[b] = 1;
  a[c] = 1;
}
/* { dg-final { scan-assembler "\tsave\t\\\$4-\\\$6," } } */
/* { dg-final { scan-assembler "\trestore\t" } } */
