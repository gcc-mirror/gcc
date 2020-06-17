/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#ifndef t
#define t short int
#endif
#ifndef t2
#define t2 int
#endif

#define eq_op(x) ((x) == 0)
#define ne_op(x) ((x) != 0)
#define gt_op(x) ((x) > 0)
#define gtu_op(x) ((x) > 0)
#define lt_op(x) ((x) < 0)
#define ltu_op(x) ((x) < 0)
#define ge_op(x) ((x) >= 0)
#define geu_op(x) ((x) >= 0)
#define le_op(x) ((x) <= 0)
#define leu_op(x) ((x) <= 0)

#define f(n, T, T2) \
T2 f ## n(T *a, T *b, T2 *d) \
{ \
  T2 c = *a; \
  *d = c; \
  *b = n ## _op (c); \
  return c; \
}

f(eq, t, t2)
f(ne, t, t2)
f(gt, t, t2)
f(gtu, unsigned t, unsigned t2)
f(lt, t, t2)
#if 0
f(ltu, unsigned t, unsigned t2)
#endif
f(ge, t, t2)
#if 0
f(geu, t, t2)
#endif
f(le, t, t2)
f(leu, t, t2)
