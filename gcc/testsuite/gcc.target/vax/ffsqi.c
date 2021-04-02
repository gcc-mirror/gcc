/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (QI))) int_t;

int
ffsqi (int_t *x)
{
  return __builtin_ffs (*x);
}

/* Expect assembly like:

	ffs $0,$8,*4(%ap),%r0
	jneq .L2
	mnegl $1,%r0
.L2:
	incl %r0

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler "\tffs \\\$0,\\\$8," } } */
