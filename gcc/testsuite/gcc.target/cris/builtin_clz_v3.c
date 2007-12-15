/* Check that we use the lz insn for clz by checking assembler output.
   The lz insn was implemented in CRIS v3 (ETRAX 4).  */
/* { dg-do compile } */
/* { dg-skip-if "" { "cris*-*-elf" } { "-march*" } { "" } } */
/* { dg-options "-O2 -march=v3" } */
/* { dg-final { scan-assembler "\[ \t\]lz\[ \t\]" } } */

int
f (int a)
{
	return __builtin_clz(a);
}
