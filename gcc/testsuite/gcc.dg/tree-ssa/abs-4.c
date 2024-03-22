/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* { dg-additional-options "-msse -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* PR tree-optimization/109829 */

float abs_f(float x) { return __builtin_signbit(x) ? x : -x; }
double abs_d(double x) { return __builtin_signbit(x) ? x : -x; }
long double abs_ld(long double x) { return __builtin_signbit(x) ? x : -x; }


/* __builtin_signbit(x) ? x : -x. Should be convert into - ABS_EXP<x> */
/* { dg-final { scan-tree-dump-not "signbit" "optimized"} } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 1 "optimized" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times "= -" 1 "optimized" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times "= \.COPYSIGN" 2 "optimized" { target { ifn_copysign && { ! { s390*-*-* } } } } } } */
/* { dg-final { scan-tree-dump-times "= \.COPYSIGN" 3 "optimized" { target { ifn_copysign && s390*-*-* } } } } */
/* { dg-final { scan-tree-dump-times "= ABS_EXPR" 3 "optimized" { target { ! ifn_copysign } } } } */
/* { dg-final { scan-tree-dump-times "= -" 3 "optimized" { target { ! ifn_copysign } } } } */
