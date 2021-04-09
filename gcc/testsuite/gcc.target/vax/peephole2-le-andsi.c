/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
le_andsi (int_t x, int_t y)
{
  x &= y;
  if (x <= 0)
    return 1;
  else
    return 2;
}

/* Expect assembly like:

	bitl 4(%ap),8(%ap)		# 58	[c=34]  *bitsi_ccnz
	jgtr .L6			# 47	[c=26]  *branch_ccnz
	movl $1,%r0			# 42	[c=4]  *movsi_2
	ret				# 53	[c=0]  return
.L6:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(cmpz?|tst). " } } */
/* { dg-final { scan-assembler "bitsi\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccnz\n" } } */
