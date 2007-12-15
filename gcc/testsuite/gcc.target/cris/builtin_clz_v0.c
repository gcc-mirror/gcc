/* Check that we don't use the lz insn for clz by checking assembler output.
   The lz insn was implemented in CRIS v3 (ETRAX 4).  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v0" } */
/* { dg-final { scan-assembler-not "\[ \t\]lz\[ \t\]" } } */

int
f (int a)
{
	return __builtin_clz(a);
}
