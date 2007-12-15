/* Check that we don't use the swap insn for ctz by checking
   assembler output.  The swap instruction was implemented in v8.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v3" } */
/* { dg-final { scan-assembler-not "\[ \t\]swapwbr\[ \t\]" } } */

int
f (int a)
{
	return __builtin_ctz(a);
}
