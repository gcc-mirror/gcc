/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

void
lt_iorqi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x | *y;
  if (v < 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	bisb3 *12(%ap),*8(%ap),%r0	# 28	[c=44]  *iorqi3_ccn/2
	jlss .L2			# 30	[c=26]  *branch_ccn
	addb2 $2,%r0			# 27	[c=32]  *addqi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "iorqi\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
