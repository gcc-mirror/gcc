/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;
typedef int __attribute__ ((mode (QI))) short_t;

int_t
eq_ashrsi (int_t x, short_t y)
{
  x >>= y;
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	mnegb 8(%ap),%r0		# 36	[c=16]  *negqi2
	ashl %r0,4(%ap),%r0		# 37	[c=52]  *ashlnegsi3_2_ccz
	jeql .L1			# 39	[c=26]  *branch_ccz
	addl2 $2,%r0			# 35	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "ashlnegsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
