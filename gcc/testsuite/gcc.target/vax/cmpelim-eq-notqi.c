/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

void
eq_notqi (int_t *w, int_t *x)
{
  int_t v;

  v = ~*x;
  if (v == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	mcomb *8(%ap),%r0		# 31	[c=24]  *one_cmplqi2_ccz
	jeql .L2			# 33	[c=26]  *branch_ccz
	addb2 $2,%r0			# 30	[c=32]  *addqi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "one_cmplqi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
