/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

void
eq_ctzhi (int_t *w, int_t *x)
{
  int_t v;

  v = __builtin_ctz (*x + 1);
  if (v == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	cvtwl *8(%ap),%r0		# 34	[c=28]  *extendhisi2
	incl %r0			# 35	[c=32]  *addsi3
	ffs $0,$32,%r0,%r0		# 36	[c=4]  *ctzsi2
	tstl %r0			# 37	[c=6]  *cmpsi_ccz/0
	jeql .L2			# 38	[c=26]  *branch_ccz
	addw2 $2,%r0			# 33	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-not "Splitting with gen_peephole2" "peephole2" } } */
/* { dg-final { scan-assembler "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "cmpsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
