/* { dg-do compile { target scalar_all_fma } } */
/* { dg-options "-O -fgimple -fdump-tree-ssa-gimple" } */

#define foo(type, num) \
type __GIMPLE () foo_##num (type a, type b, type c) \
{ \
  type t0; \
  t0_1 = .FMA (a, b, c); \
  return t0_1; \
}

foo(float, 1)
foo(double, 2)

/* { dg-final { scan-tree-dump-times {\.FMA} 2 "ssa" } } */
