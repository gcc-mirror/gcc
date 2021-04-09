/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

void
le_andqi (int_t *w, int_t *x, int_t *y)
{
  int_t v;

  v = *x & *y;
  if (v <= 0)
    *w = 1;
  else
    *w = 2;
}

/* Expect assembly like:

	bitb *8(%ap),*12(%ap)		# 56	[c=50]  *bitqi_ccnz
	jleq .L6			# 46	[c=26]  *branch_ccnz
	movb $2,%r0			# 41	[c=4]  *movqi
	movb %r0,*4(%ap)		# 40	[c=4]  *movqi
	ret				# 52	[c=0]  return
.L6:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(cmpz?|tst). " } } */
/* { dg-final { scan-assembler "bitqi\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccnz\n" } } */
