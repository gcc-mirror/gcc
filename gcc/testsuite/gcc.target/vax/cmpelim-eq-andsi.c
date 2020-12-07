/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
eq_andsi (int_t x, int_t y)
{
  x &= ~y;
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	bicl3 8(%ap),4(%ap),%r0		# 35	[c=28]  *andsi3_2_ccz/1
	jeql .L1			# 37	[c=26]  *branch_ccz
	addl2 $2,%r0			# 34	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "andsi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
