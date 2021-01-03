/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

extern char __attribute__ ((weak)) c;

char *
eq_mova (char *p)
{
  char *v;

  v = &c;
  if (v)
    return v;
  return p;
}

/* Expect assembly like:

	movab c,%r0			# 35	[c=12]  *movsym_2_ccz
	jeql .L6			# 37	[c=26]  *branch_ccz
	ret				# 43	[c=0]  return
.L6:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "movsym\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
