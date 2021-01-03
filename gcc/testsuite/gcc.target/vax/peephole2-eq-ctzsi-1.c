/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
eq_ctzsi (int_t x)
{
  int_t v;

  v = __builtin_ctz (x + 1);
  if (v == 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	addl3 4(%ap),$1,%r0		# 33	[c=40]  *addsi3
	ffs $0,$32,%r0,%r0		# 34	[c=4]  *ctzsi2
	tstl %r0			# 35	[c=6]  *cmpsi_ccz/0
	jeql .L1			# 36	[c=26]  *branch_ccz
	addl2 $2,%r0			# 32	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-not "Splitting with gen_peephole2" "peephole2" } } */
/* { dg-final { scan-assembler "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "cmpsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
