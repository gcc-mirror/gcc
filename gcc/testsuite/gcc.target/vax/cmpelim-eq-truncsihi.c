/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef unsigned int __attribute__ ((mode (SI))) int_t;
typedef unsigned int __attribute__ ((mode (HI))) short_t;

void
eq_truncsihi (short_t *w, int_t *x, int y)
{
  short_t v;

  v = x[y];
  if (v == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	movl 12(%ap),%r0		# 33	[c=16]  *movsi_2
	cvtlw *8(%ap)[%r0],%r0		# 34	[c=28]  *truncsihi2_ccz
	jeql .L2			# 36	[c=26]  *branch_ccz
	addw2 $2,%r0			# 32	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "truncsihi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
