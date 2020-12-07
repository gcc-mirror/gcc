/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
eq_ffssi (int_t x)
{
  return __builtin_ffs (x);
}

/* Expect assembly like:

	movl 4(%ap),%r1			# 28	[c=16]  *movsi_2
	ffs $0,$32,%r1,%r0		# 41	[c=4]  *ctzsi2_ccz
	jneq .L2			# 31	[c=26]  *branch_ccz
	mnegl $1,%r0			# 27	[c=8]  *negsi2
.L2:
	incl %r0			# 26	[c=32]  *addsi3

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ctzsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
