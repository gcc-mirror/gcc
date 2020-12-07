/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef signed int __attribute__ ((mode (SI))) int_t;
typedef struct
  {
    int_t h : 7;
    int_t i : 18;
    int_t l : 7;
  }
bit_t;

int_t
eq_cmpvsi (bit_t x, int_t y)
{
  if (x.i == y)
    return 1;
  else
    return 2;
}

/* Expect assembly like:

	cmpv $7,$18,4(%ap),8(%ap)	# 50	[c=88]  *cmpv_ccz
	jeql .L3			# 39	[c=26]  *branch_ccz
	movl $2,%r0			# 36	[c=4]  *movsi_2
	ret				# 31	[c=0]  return
.L3:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmp|tst)\[bwl\] " } } */
/* { dg-final { scan-assembler "cmpv\[^ \]*_ccz(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_ccz\n" } } */
