/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef unsigned int __attribute__ ((mode (QI))) int_t;

void
leu_subqi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x - *y;
  if (*x <= *y)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movb *8(%ap),%r2		# 28	[c=24]  *movqi
	movb *12(%ap),%r1		# 29	[c=24]  *movqi
	subb3 %r1,%r2,%r0		# 30	[c=32]  *subqi3_cc/1
	jlequ .L2			# 32	[c=26]  *branch_cc
	addb2 $2,%r0			# 27	[c=32]  *addqi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "subqi\[^ \]*_cc(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_cc\n" } } */
