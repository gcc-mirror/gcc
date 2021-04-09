/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

void
eq_mulhi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x * *y;
  if (v == 0)
    *w = v;
  else
    *w = v + 2;
}

/* Expect assembly like:

	mulw3 *8(%ap),*12(%ap),%r0	# 33	[c=72]  *mulhi3_ccz/2
	jeql .L2			# 35	[c=26]  *branch_ccz
	addw2 $2,%r0			# 32	[c=32]  *addhi3
.L2:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "mulhi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
