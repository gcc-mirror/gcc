/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-O -fgimple -fdump-tree-ssa-gimple" } */

#define foo(type, num) \
type __GIMPLE () foo_##num (type a, type b, type c) \
{ \
  type t0; \
  t0_1 = __FMA (a, b, c); \
  return t0_1; \
}

foo(float, 1)
foo(double, 2)
foo(long double, 3)

/* { dg-final { scan-tree-dump-times "__FMA" 3 "ssa" } } */
