/* Check that we don't try to save the same register twice.  */
/* { dg-options "(-mips16) isa_rev>=1 -mgp32" } */

int bar (int, int, int, int);
void frob (void);

MIPS16 void
foo (int a1, int a2, int a3, int a4)
{
  asm volatile ("" ::: "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		"$9", "$10", "$11", "$12", "$13", "$14", "$15", "$16",
		"$18", "$19", "$20", "$21", "$22", "$23", "$24",
		"$25", "$30", "$31", "memory");
  __builtin_eh_return (bar (a1, a2, a3, a4), frob);
}
