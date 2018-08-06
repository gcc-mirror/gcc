/* PR tree-optimization/86401 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " \\+ 64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64 - " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\+ -64" "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 319" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| -128" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\^ -384" "optimized" } } */

int f1 (int x) { int m = 64; return (m + x) & (m - 1); }
int f2 (int x) { return (64 + x) & 63; }
int f3 (int x) { int m = 64; return (x + m) & (m - 1); }
int f4 (int x) { return (x + 64) & 63; }
int f5 (int x) { int m = 64; return (m - x) & (m - 1); }
int f6 (int x) { return (64 - x) & 63; }
int f7 (int x) { int m = 64; return (x - m) & (m - 1); }
int f8 (int x) { return (x - 64) & 63; }
int f9 (int x, int y) { int m = 64, n = 256 | (m - 1); return ((x & n) + y) & (m - 1); }
int f10 (int x, int y) { return ((x & 319) + y) & 63; }
int f11 (int x, int y) { int m = 64, n = -128; return ((x | n) + y) & (m - 1); }
int f12 (int x, int y) { return ((x | -128) + y) & 63; }
int f13 (int x, int y) { int m = 64, n = -384; return ((x ^ n) + y) & (m - 1); }
int f14 (int x, int y) { return ((x ^ -384) + y) & 63; }
int f15 (int x, int y) { int m = 64, n = 256 | (m - 1); return (y + (x & n)) & (m - 1); }
int f16 (int x, int y) { return (y + (x & 319)) & 63; }
int f17 (int x, int y) { int m = 64, n = -128; return (y + (x | n)) & (m - 1); }
int f18 (int x, int y) { return (y + (x | -128)) & 63; }
int f19 (int x, int y) { int m = 64, n = -384; return (y + (x ^ n)) & (m - 1); }
int f20 (int x, int y) { return (y + (x ^ -384)) & 63; }
int f21 (int x, int y) { int m = 64, n = 256 | (m - 1); return ((x & n) - y) & (m - 1); }
int f22 (int x, int y) { return ((x & 319) - y) & 63; }
int f23 (int x, int y) { int m = 64, n = -128; return ((x | n) - y) & (m - 1); }
int f24 (int x, int y) { return ((x | -128) - y) & 63; }
int f25 (int x, int y) { int m = 64, n = -384; return ((x ^ n) - y) & (m - 1); }
int f26 (int x, int y) { return ((x ^ -384) - y) & 63; }
int f27 (int x, int y) { int m = 64, n = 256 | (m - 1); return (y - (x & n)) & (m - 1); }
int f28 (int x, int y) { return (y - (x & 319)) & 63; }
int f29 (int x, int y) { int m = 64, n = -128; return (y - (x | n)) & (m - 1); }
int f30 (int x, int y) { return (y - (x | -128)) & 63; }
int f31 (int x, int y) { int m = 64, n = -384; return (y - (x ^ n)) & (m - 1); }
int f32 (int x, int y) { return (y - (x ^ -384)) & 63; }
int f33 (int x) { int m = 64, n = 256 | (m - 1); return (-(x & n)) & (m - 1); }
int f34 (int x) { return (-(x & 319)) & 63; }
int f35 (int x) { int m = 64, n = -128; return (-(x | n)) & (m - 1); }
int f36 (int x) { return (-(x | -128)) & 63; }
int f37 (int x) { int m = 64, n = -384; return (-(x ^ n)) & (m - 1); }
int f38 (int x) { return (-(x ^ -384)) & 63; }
