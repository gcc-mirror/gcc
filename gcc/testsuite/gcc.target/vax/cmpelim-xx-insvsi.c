/* { dg-do compile } */
/* { dg-options "-fdump-rtl-cmpelim -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

typedef signed int __attribute__ ((mode (SI))) int_t;
typedef union
  {
    int_t i;
    struct
      {
	int_t h : 7;
	int_t i : 18;
	int_t l : 7;
      } b;
  }
bit_t;

int
xx_insvsi (bit_t x, int_t y)
{
  int_t v;

  v = x.b.i;
  x.b.i = y;
  if (v == 0)
    return x.i;
  else if (v >= 0)
    return x.i + 2;
  else
    return x.i - 3;
}

/* Expect assembly like:

	movl 4(%ap),%r0			# 50	[c=16]  *movsi_2
	extv $7,$18,%r0,%r1		# 51	[c=60]  *extv_non_const_2_ccnz
	insv 8(%ap),$7,$18,%r0		# 8	[c=16]  *insv_2
	jeql .L1			# 53	[c=26]  *branch_ccz
	jlss .L4			# 49	[c=26]  *branch_ccn
	addl2 $2,%r0			# 47	[c=32]  *addsi3
	ret				# 42	[c=0]  return
.L4:
	subl2 $3,%r0			# 46	[c=32]  *addsi3
.L1:

 */

/* { dg-final { scan-rtl-dump-times "deleting insn with uid" 2 "cmpelim" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmpz?|tst). " } } */
/* { dg-final { scan-assembler "extv\[^ \]*_ccnz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "extv.*insv.*branch" } } */
/* { dg-final { scan-assembler "branch_ccn\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
