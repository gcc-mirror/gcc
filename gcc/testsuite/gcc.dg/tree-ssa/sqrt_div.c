/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */
/* { dg-require-effective-target c99_runtime } */

#define T(n, type, fname)          \
type f##n (type x)                 \
{                                  \
  type t1 = __builtin_##fname (x); \
  type t2 = x / t1;                \
  return t2;                       \
}                               

T(1, double, sqrt)

/* { dg-final { scan-tree-dump "gimple_simplified to t2_\[0-9\]+ = __builtin_sqrt .x_\[0-9\]*.D.." "forwprop1" } }     */

T(2, float, sqrtf )

/* { dg-final { scan-tree-dump "gimple_simplified to t2_\[0-9\]+ = __builtin_sqrtf .x_\[0-9\]*.D.." "forwprop1" } }     */

T(3, long double, sqrtl)

/* { dg-final { scan-tree-dump "gimple_simplified to t2_\[0-9\]+ = __builtin_sqrtl .x_\[0-9\]*.D.." "forwprop1" } } */
