/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
xx_addsi (int_t x, int_t y)
{
  x += y;
  if (x == 0)
    return x;
  else if (x >= 0)
    return x + 2;
  else
    return x - 3;
}

/* Expect assembly like:

	addl3 4(%ap),8(%ap),%r0		# 47	[c=48]  *addsi3_ccnz
	jeql .L1			# 49	[c=26]  *branch_ccz
	jlss .L3			# 46	[c=26]  *branch_ccn
	addl2 $2,%r0			# 44	[c=32]  *addsi3
	ret				# 39	[c=0]  return
.L3:
	subl2 $3,%r0			# 43	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 2 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "addsi\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
