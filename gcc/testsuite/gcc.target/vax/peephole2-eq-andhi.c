/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

void
eq_andhi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x & *y;
  if (v == 0)
    *w = 1;
  else
    *w = 2;
}

/* Expect assembly like:

	bitw *8(%ap),*12(%ap)		# 50	[c=50]  *bithi_ccz
	jneq .L3			# 40	[c=26]  *branch_ccz
	movw $1,%r0			# 36	[c=4]  *movhi
	movw %r0,*4(%ap)		# 34	[c=4]  *movhi
	ret				# 46	[c=0]  return
.L3:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(cmpz?|tst). " } } */
/* { dg-final { scan-assembler "bithi\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
