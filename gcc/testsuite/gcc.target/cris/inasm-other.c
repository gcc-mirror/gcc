/* { dg-do assemble } */
/* The base test-case is sort-of-disabled or rather made
   always-pass, but remains included by other tests. */

/* Make sure we can (generate code and) assemble for the "other"
   variant, with the twist that the gcc option -march=v0 isn't
   valid for the assembler.  We don't check that the generated code
   is for the other variant; other tests cover that already, but they
   don't *assemble* the result.  We can't trust the prologue and
   epilogue to contain incompatible insns (they actually deliberately
   don't, usually and it'd be brittle to tweak the function signature
   to make it so), so we force some with inline asm.  */

void f(void)
{
#ifdef OTHER_ISA
  asm volatile ("0: move.d [$r12=$sp+42],$r10\n\t"
		"bwf 0b\n\t"
		"nop");
#endif
}
