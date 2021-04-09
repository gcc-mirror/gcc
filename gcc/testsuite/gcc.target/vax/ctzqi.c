/* { dg-do compile } */
/* { dg-options "-fdump-rtl-peephole2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */

typedef unsigned int __attribute__ ((mode (QI))) int_t;

int
ctzqi (int_t *x)
{
  return __builtin_ctz (*x);
}

/* Expect assembly like:

	ffs $0,$8,*4(%ap),%r0

 */

/* { dg-final { scan-rtl-dump-times "Splitting with gen_peephole2" 1 "peephole2" } } */
/* { dg-final { scan-assembler "\tffs \\\$0,\\\$8," } } */
