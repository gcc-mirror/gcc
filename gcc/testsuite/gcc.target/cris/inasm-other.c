/* { dg-do assemble } */
/* { dg-options "-DOTHER_ISA=0 -march=v0" { target crisv32-*-* } } */
/* { dg-options "-DOTHER_ISA=32 -march=v32" { target cris-*-* } } */

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
#if OTHER_ISA == 32
  asm volatile ("addoq 42,$r11,$acr");
#else
  asm volatile ("0: move.d [$r12=$sp+42],$r10\n\t"
		"bwf 0b\n\t"
		"nop");
#endif
}
