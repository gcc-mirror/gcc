/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
lt_andsi (int_t x, int_t y)
{
  x &= y;
  if (x < 0)
    return 1;
  else
    return 2;
}

/* Expect assembly like:

	bitl 4(%ap),8(%ap)		# 68	[c=34]  *bitsi_ccn
	jgeq .L6			# 57	[c=26]  *branch_ccn
	movl $1,%r0			# 52	[c=4]  *movsi_2
	ret				# 63	[c=0]  return
.L6:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(cmpz?|tst). " } } */
/* { dg-final { scan-assembler "bitsi\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
