/* PR tree-optimization/112994 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 2;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 7;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return -7;" 2 "optimized" } } */

int f1 (int x) { return (x * 4) / (x * 2); }
int f2 (int x) { return (x * 56) / (x * 8); }
int f3 (int x) { return (x * 56) / (x * -8); }
int f4 (int x) { int y = x * 4; return y / (x * 2); }
int f5 (int x) { int y = x * 56; return y / (x * 8); }
int f6 (int x) { int y = x * 56; return y / (x * -8); }
unsigned f7 (unsigned x) { if (x > ~0U / 4) __builtin_unreachable (); unsigned y = x * 4; return y / (x * 2); }
unsigned f8 (unsigned x) { if (x > ~0U / 56) __builtin_unreachable (); unsigned y = x * 56; return y / (x * 8); }
