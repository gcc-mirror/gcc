/* { dg-do compile } */
/* { dg-options "-O1 -mabi=call0 -fdump-rtl-reload" } */

void test(void)
{
  asm volatile ("# clobber a2~a15, reg %0" : : "r"(0) :
		"a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9",
		"a10", "a11", "a12", "a13", "a14", "a15");
}

/* { dg-final { scan-rtl-dump-not "Changing spilled pseudos to memory in insn #" "reload" } } */
