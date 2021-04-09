/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;

float_t
eq_mulsf (float_t x, float_t y)
{
  x *= y;
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	mulf3 4(%ap),8(%ap),%r0		# 34	[c=52]  *mulsf3_ccz/2
	jeql .L1			# 36	[c=26]  *branch_ccz
	addf2 $0f2.0e+0,%r0		# 33	[c=36]  *addsf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "mulsf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
