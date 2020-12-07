/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2 -dp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef unsigned int __attribute__ ((mode (SI))) uint_t;
typedef int __attribute__ ((mode (SI))) int_t;

uint_t
ltu_cmpzvsi (uint_t x, int_t y, uint_t z)
{
  if (x >> y < z)
    return 1;
  else
    return 2;
}

/* Expect assembly like:

	subb3 8(%ap),$32,%r0		# 39	[c=40]  *subqi3/1
	cmpzv 8(%ap),%r0,4(%ap),12(%ap)	# 53	[c=96]  *cmpzv_cc
	jgequ .L6			# 42	[c=26]  *branch_cc
	movl $1,%r0			# 37	[c=4]  *movsi_2
	ret				# 48	[c=0]  return
.L6:

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler-not "\t(bit|cmp|tst)\[bwl\] " } } */
/* { dg-final { scan-assembler "cmpzv\[^ \]*_cc(/\[0-9\]+)?\n" } } */
/* { dg-final { scan-assembler "branch_cc\n" } } */
