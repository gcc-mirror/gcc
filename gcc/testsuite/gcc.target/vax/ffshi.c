/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef int __attribute__ ((mode (HI))) int_t;

int
ffshi (int_t *x)
{
  return __builtin_ffs (*x);
}

/* Expect assembly like:

	ffs $0,$16,*4(%ap),%r0
	jneq .L2
	mnegl $1,%r0
.L2:
	incl %r0

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler "\tffs \\\$0,\\\$16," } } */
