/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (DF))) float_t;

float_t
eq_movdf (float_t x)
{
  if (x == 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	movd 4(%ap),%r0			# 34	[c=24]  *movdf_ccz/1
	jeql .L2			# 36	[c=26]  *branch_ccz
	addd2 $0d2.0e+0,%r0		# 33	[c=56]  *adddf3/0
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "movdf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
