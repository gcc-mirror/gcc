/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;
typedef int __attribute__ ((mode (SI))) int_t;

float_t
eq_floatsisf (int_t x)
{
  float_t v;

  v = x;
  if (v == 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	cvtlf 4(%ap),%r0		# 33	[c=32]  *floatsisf2_ccz
	jeql .L1			# 35	[c=26]  *branch_ccz
	addf2 $0f2.0e+0,%r0		# 32	[c=36]  *addsf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "floatsisf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
