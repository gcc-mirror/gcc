/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

int sink;

/* C1 + C2 reaches the sign bit.  */
int m1 (int x) { return ((x + 0x40000000) | (0x40000000 - x)) < 0; }
/* C1 + C2 is negative.  */
int m2 (int x) { return ((x + -5) | (-5 - x)) < 0; }
/* The disjunction has a second use.  */
int m3 (int x) { int t = (x + 5) | (5 - x); sink = t; return t < 0; }
/* The ABS_EXPR has a second use.  */
int m4 (int x) { int t = __builtin_abs (x); sink = t; return t > 5; }

/* { dg-final { scan-tree-dump-times " \\| " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR" 1 "optimized" } } */
