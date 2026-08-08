/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow -fdump-tree-optimized" } */

int  f (int x, int y)   { return __builtin_abs  (x) * __builtin_abs  (y); }
long g (long x, long y) { return __builtin_labs (x) * __builtin_labs (y); }

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 4 "optimized" } } */
