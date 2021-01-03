/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
eq_notsi (int_t x)
{
  x = ~x;
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	mcoml 4(%ap),%r0		# 32	[c=16]  *one_cmplsi2_ccz
	jeql .L1			# 34	[c=26]  *branch_ccz
	addl2 $2,%r0			# 31	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "one_cmplsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
