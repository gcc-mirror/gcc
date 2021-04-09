/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

void
eq_ctzqi (int_t *w, int_t *x)
{
  int_t v;

  v = __builtin_ctz (*x);
  if (*x == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movb *8(%ap),%r1		# 34	[c=24]  *movqi
	ffs $0,$8,%r1,%r0		# 49	[c=4]  *ctzqi2_ccz
	jeql .L3			# 38	[c=26]  *branch_ccz
	addb2 $2,%r0			# 33	[c=32]  *addqi3
.L3:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 2 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ctzqi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
