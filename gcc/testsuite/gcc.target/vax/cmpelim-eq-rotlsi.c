/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef unsigned int __attribute__ ((mode (SI))) ulong_t;
typedef int __attribute__ ((mode (SI))) long_t;
typedef int __attribute__ ((mode (QI))) int_t;

ulong_t
eq_rotlsi (ulong_t x, int_t y)
{
  long_t v;

  v = x << y | x >> 8 * sizeof (x) - y;
  if (v == 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	rotl 8(%ap),4(%ap),%r0		# 36	[c=40]  *rotlsi3_ccz
	jeql .L1			# 38	[c=26]  *branch_ccz
	addl2 $2,%r0			# 35	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "rotlsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
