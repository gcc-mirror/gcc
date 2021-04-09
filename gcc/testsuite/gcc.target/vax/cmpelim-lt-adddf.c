/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef float __attribute__ ((mode (DF))) float_t;

float_t
lt_adddf (float_t x, float_t y)
{
  x += y;
  if (x < 0)
    return x;
  else
    return x + 2;
}

/* Expect assembly like:

	addd3 4(%ap),12(%ap),%r0	# 29	[c=68]  *adddf3_ccn/2
	jlss .L1			# 31	[c=26]  *branch_ccn
	addd2 $0d2.0e+0,%r0		# 28	[c=56]  *adddf3/0
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 1 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "adddf\[^ \]*_ccn(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
