/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

int f1 (int x) { return ((x + 1) | (1 - x)) < 0; }
int f2 (int x) { return ((x + 5) | (5 - x)) >= 0; }
int f3 (int x) { return (((x + 1) | (1 - x)) >> 31) & 1; }
int f4 (int x) { return ((x - 3) | (9 - x)) < 0; }
int f5 (int c) { return ((c - 'a') | ('z' - c)) < 0; }
int f6 (unsigned int x) { return (int) ((x - 3) | (9 - x)) < 0; }
int f7 (short x) { return (short) ((x - 3) | (9 - x)) < 0; }
/* C1 == 0 and C2 == 0 are spelled without the addition and as a negate.  */
int f8 (int x) { return (x | (6 - x)) < 0; }
int f9 (int x) { return ((x + 6) | -x) < 0; }
int f10 (int x) { return (x | -x) < 0; }
int f11 (int x) { return (unsigned) (x | -x) >> 31; }
int f12 (int x) { return __builtin_abs (x) > 5; }
int f13 (int x) { return __builtin_abs (x) >= 6; }
int f14 (int x) { return __builtin_abs (x) <= 5; }
int f15 (int x) { return __builtin_abs (x) < 6; }

/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 2;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 6;" 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 10;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 10;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 25;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " != 0;" 2 "optimized" } } */
