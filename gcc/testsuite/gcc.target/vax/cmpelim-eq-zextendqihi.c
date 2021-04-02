/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef unsigned int __attribute__ ((mode (HI))) int_t;
typedef unsigned int __attribute__ ((mode (QI))) short_t;

void
eq_zextendqihi (int_t *w, int_t *x)
{
  int_t v;

  v = (short_t) *x;
  if (v == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movzbw *8(%ap),%r0		# 31	[c=28]  *zero_extendqihi2_ccz
	jeql .L2			# 33	[c=26]  *branch_ccz
	addw2 $2,%r0			# 30	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "zero_extendqihi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
