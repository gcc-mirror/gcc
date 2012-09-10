/* Check that we can use the save instruction to save spilled arguments
   when the argument save area is out of range of a direct load or store.  */
/* { dg-options "(-mips16) isa_rev>=1 -mabi=32" } */

void bar (int *);

MIPS16 void
foo (int *a, int b, int c)
{
  int x[0x4000];
  asm volatile ("" ::: "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		"$9", "$10", "$11", "$12", "$13", "$14", "$15", "$16",
		"$18", "$19", "$20", "$21", "$22", "$23", "$24",
		"$25", "$30", "memory");
  bar (x);
  a[b] = 1;
  a[c] = 1;
}
/* { dg-final { scan-assembler "\tsave\t\\\$4-\\\$6," } } */
/* { dg-final { scan-assembler "\trestore\t" } } */
