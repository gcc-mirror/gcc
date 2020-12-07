/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;
typedef int __attribute__ ((mode (HI))) int_t;

void
lt_fixsfhi (int_t *w, float_t x)
{
  int_t v;

  v = x;
  if (v < 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	cvtfw 8(%ap),%r0		# 27	[c=36]  *fix_truncsfhi2_ccn
	jlss .L2			# 29	[c=26]  *branch_ccn
	addw2 $2,%r0			# 26	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "fix_truncsfhi\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
