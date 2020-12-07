/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef unsigned int __attribute__ ((mode (SI))) int_t;

int_t
leu_subsi (int_t x, int_t y)
{
  int_t v;

  v = x - y;
  if (x <= y)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	movl 4(%ap),%r2			# 27	[c=16]  *movsi_2
	movl 8(%ap),%r1			# 28	[c=16]  *movsi_2
	subl3 %r1,%r2,%r0		# 29	[c=32]  *subsi3_cc/1
	jlequ .L1			# 31	[c=26]  *branch_cc
	addl2 $2,%r0			# 26	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "subsi\[^ \]*_cc(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_cc\n" } } */
