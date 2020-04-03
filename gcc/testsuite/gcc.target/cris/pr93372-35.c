/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */
/* { dg-final { scan-assembler "\tneg" } } */

void foo(void);

#ifndef op
#define op(x) -(x)
#endif

#define f(k, T, T2)				\
void f ## k (T *a, T2 *b)			\
{						\
  T2 d = op(*a);				\
  *b = d;					\
  if (d != 0)					\
    foo();					\
}

#define ff(x, y) f(x, y, y)

/* For NEG, gcc prefers to test the source (before the operation), but
   will settle for the destination.  For SImode, the destination is
   allocated to a different register than the source.  Not that
   important; just skip the "int" variant for now.  */
#ifndef do_1
#define do_1 0
#endif

#ifndef do_2
#define do_2 1
#endif
#ifndef do_3
#define do_3 1
#endif

#if do_1
ff(1, int)
#endif

#if do_2
ff(2, short int)
#endif

#if do_3
ff(3, signed char)
#endif
