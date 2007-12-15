/* Check that we use the swap insn for bswap by checking assembler
   output.  The swap instruction was added in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v8" } */
/* { dg-final { scan-assembler "\[ \t\]swapwb\[ \t\]" } } */

int
f (int a)
{
	return __builtin_bswap32(a);
}
