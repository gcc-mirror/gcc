/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;

float_t
eq_movsf (float_t x)
{
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	movf 4(%ap),%r0			# 33	[c=16]  *movsf_ccz/1
	jeql .L2			# 35	[c=26]  *branch_ccz
	addf2 $0f2.0e+0,%r0		# 32	[c=36]  *addsf3/0
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "movsf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
