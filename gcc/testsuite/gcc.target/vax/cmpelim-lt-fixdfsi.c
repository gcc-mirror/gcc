/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (DF))) float_t;
typedef int __attribute__ ((mode (SI))) int_t;

int_t
lt_fixdfsi (float_t x)
{
  int_t v;

  v = x;
  if (v < 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	cvtdl 4(%ap),%r0		# 28	[c=36]  *fix_truncdfsi2_ccn
	jlss .L1			# 30	[c=26]  *branch_ccn
	addl2 $2,%r0			# 27	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "fix_truncdfsi\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
