/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;

float_t
lt_divsf (float_t x, float_t y)
{
  x /= y;
  if (x < 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	divf3 8(%ap),4(%ap),%r0		# 28	[c=60]  *divsf3_ccn/1
	jlss .L1			# 30	[c=26]  *branch_ccn
	addf2 $0f2.0e+0,%r0		# 27	[c=36]  *addsf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "divsf\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
