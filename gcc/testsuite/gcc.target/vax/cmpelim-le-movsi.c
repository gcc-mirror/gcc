/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef int __attribute__ ((mode (SI))) int_t;

int_t
le_movsi (int_t x)
{
  if (x <= 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	movl 4(%ap),%r0			# 34	[c=16]  *movsi_2_ccnz
	jleq .L1			# 36	[c=26]  *branch_ccnz
	addl2 $2,%r0			# 33	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "movsi\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccnz\n" } } */
