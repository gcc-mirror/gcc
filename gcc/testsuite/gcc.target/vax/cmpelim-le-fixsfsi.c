/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (SF))) float_t;
typedef int __attribute__ ((mode (SI))) int_t;

int_t
le_fixsfsi (float_t x)
{
  int_t v;

  v = x;
  if (v <= 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	cvtfl 4(%ap),%r0		# 28	[c=36]  *fix_truncsfsi2_ccnz
	jleq .L1			# 30	[c=26]  *branch_ccnz
	addl2 $2,%r0			# 27	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "fix_truncsfsi\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccnz\n" } } */
