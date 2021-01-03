/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (DF))) float_t;

float_t
eq_divdf (float_t x, float_t y)
{
  x /= y;
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	divd3 12(%ap),4(%ap),%r0	# 35	[c=112]  *divdf3_ccz/1
	jeql .L1			# 37	[c=26]  *branch_ccz
	addd2 $0d2.0e+0,%r0		# 34	[c=56]  *adddf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "divdf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
