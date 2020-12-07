/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

void
eq_ctzhi (int_t *w, int_t *x)
{
  int_t v;

  v = __builtin_ctz (*x);
  if (*x == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movw *8(%ap),%r1		# 34	[c=24]  *movhi
	ffs $0,$16,%r1,%r0		# 49	[c=4]  *ctzhi2_ccz
	jeql .L3			# 38	[c=26]  *branch_ccz
	addw2 $2,%r0			# 33	[c=32]  *addhi3
.L3:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 2 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ctzhi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
