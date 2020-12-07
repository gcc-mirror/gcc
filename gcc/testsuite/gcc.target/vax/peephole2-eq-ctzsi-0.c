/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
eq_ctzsi (int_t x)
{
  int_t v;

  v = __builtin_ctz (x);
  if (x == 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	movl 4(%ap),%r1			# 32	[c=16]  *movsi_2
	ffs $0,$32,%r1,%r0		# 45	[c=4]  *ctzsi2_ccz
	jeql .L1			# 35	[c=26]  *branch_ccz
	addl2 $2,%r0			# 31	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ctzsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
