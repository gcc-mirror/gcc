/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef unsigned int __attribute__ ((mode (HI))) int_t;

void
ltu_subhi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x - *y;
  if (*x < *y)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movw *8(%ap),%r2		# 28	[c=24]  *movhi
	movw *12(%ap),%r1		# 29	[c=24]  *movhi
	subw3 %r1,%r2,%r0		# 30	[c=32]  *subhi3_cc/1
	jlssu .L2			# 32	[c=26]  *branch_cc
	addw2 $2,%r0			# 27	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "subhi\[^ \]*_cc(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_cc\n" } } */
