/* PR tree-optimization/112994 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " / \\\[2389-\\\]" "optimized" } } */

int f1 (int x) { return (x * 4) / 2; }
int f2 (int x) { return (x * 56) / 8; }
int f3 (int x) { return (x * 56) / -8; }
int f4 (int x) { int y = x * 4; return y / 2; }
int f5 (int x) { int y = x * 56; return y / 8; }
int f6 (int x) { int y = x * 56; return y / -8; }
unsigned f7 (unsigned x) { if (x > ~0U / 6) __builtin_unreachable (); unsigned y = x * 6; return y / 3; }
unsigned f8 (unsigned x) { if (x > ~0U / 63) __builtin_unreachable (); unsigned y = x * 63; return y / 9; }
