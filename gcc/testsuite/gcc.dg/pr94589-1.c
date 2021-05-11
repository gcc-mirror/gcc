/* PR tree-optimization/94589 */
/* { dg-do compile } */
/* { dg-options "-O2 -g0 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\[ij]_\[0-9]+\\(D\\) (?:<|<=|==|!=|>|>=) \[ij]_\[0-9]+\\(D\\)" 14 "optimized" } } */
/* { dg-final { scan-tree-dump-times "i_\[0-9]+\\(D\\) (?:<|<=|==|!=|>|>=) \[45]" 14 "optimized" } } */

#define A __attribute__((noipa))
A int f1 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c == 0; }
A int f2 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c != 0; }
A int f3 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c > 0; }
A int f4 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c < 0; }
A int f5 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c >= 0; }
A int f6 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c <= 0; }
A int f7 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c == -1; }
A int f8 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c != -1; }
A int f9 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c > -1; }
A int f10 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c <= -1; }
A int f11 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c == 1; }
A int f12 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c != 1; }
A int f13 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c < 1; }
A int f14 (int i, int j) { int c = i == j ? 0 : i < j ? -1 : 1; return c >= 1; }
A int f15 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c == 0; }
A int f16 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c != 0; }
A int f17 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c > 0; }
A int f18 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c < 0; }
A int f19 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c >= 0; }
A int f20 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c <= 0; }
A int f21 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c == -1; }
A int f22 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c != -1; }
A int f23 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c > -1; }
A int f24 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c <= -1; }
A int f25 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c == 1; }
A int f26 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c != 1; }
A int f27 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c < 1; }
A int f28 (int i) { int c = i == 5 ? 0 : i < 5 ? -1 : 1; return c >= 1; }
