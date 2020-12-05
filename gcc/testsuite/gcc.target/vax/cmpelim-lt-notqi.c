/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

void
lt_notqi (int_t *w, int_t *x)
{
  int_t v;

  v = ~*x;
  if (v < 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	mcomb *8(%ap),%r0		# 27	[c=24]  *one_cmplqi2_ccn
	jlss .L2			# 29	[c=26]  *branch_ccn
	addb2 $2,%r0			# 26	[c=32]  *addqi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "one_cmplqi\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
