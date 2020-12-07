/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) single_t;
typedef float __attribute__ ((mode (DF))) double_t;

single_t
eq_truncdfsf (double_t x)
{
  single_t v;

  v = x;
  if (v == 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	cvtdf 4(%ap),%r0		# 33	[c=20]  *truncdfsf2_ccz
	jeql .L1			# 35	[c=26]  *branch_ccz
	addf2 $0f2.0e+0,%r0		# 32	[c=36]  *addsf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "truncdfsf\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
