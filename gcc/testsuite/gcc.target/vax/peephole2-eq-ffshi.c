/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

void
eq_ffshi (int_t *w, int_t *x)
{
  *w = __builtin_ffs (*x);
}

/* Expect assembly like:

	ffs $0,$16,*8(%ap),%r1		# 40	[c=28]  *ctzhi2_ccz
	jneq .L2			# 30	[c=26]  *branch_ccz
	mnegl $1,%r1			# 26	[c=8]  *negsi2
.L2:
	addw3 %r1,$1,*4(%ap)		# 25	[c=32]  *addhi3

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ctzhi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
