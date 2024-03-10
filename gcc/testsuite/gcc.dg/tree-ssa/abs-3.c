/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* PR tree-optimization/109829 */

float abs_f(float x) { return __builtin_signbit(x) ? -x : x; }
double abs_d(double x) { return __builtin_signbit(x) ? -x : x; }
long double abs_ld(long double x) { return __builtin_signbit(x) ? -x : x; }


/* __builtin_signbit(x) ? -x : x. Should be convert into ABS_EXP<x> */
/* { dg-final { scan-tree-dump-not "signbit" "optimized"} } */
/* { dg-final { scan-tree-dump-not "= -" "optimized"} } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 3 "optimized"} } */
