/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef unsigned int __attribute__ ((mode (SI))) uint_t;
typedef int __attribute__ ((mode (SI))) int_t;

uint_t
le_extzvsi (uint_t x, int_t y)
{
  int_t v;

  v = x >> y;
  if (v <= 0)
    return v;
  else
    return v + 2;
}

/* Expect assembly like:

	subb3 8(%ap),$32,%r0		# 31	[c=40]  *subqi3/1
	extzv 8(%ap),%r0,4(%ap),%r0	# 32	[c=76]  *extzv_non_const_2_ccnz
	jleq .L1			# 34	[c=26]  *branch_ccnz
	addl2 $2,%r0			# 30	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "extzv\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccnz\n" } } */
