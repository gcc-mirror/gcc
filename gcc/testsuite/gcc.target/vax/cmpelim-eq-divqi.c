/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (QI), vector_size (1))) int_t;

void
eq_divqi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x / *y;
  if (v[0] == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	divb3 *12(%ap),*8(%ap),%r0	# 38	[c=76]  *divqi3_ccz/1
	jeql .L2			# 40	[c=26]  *branch_ccz
	addb2 $2,%r0			# 37	[c=32]  *addqi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "divqi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
