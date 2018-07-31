/* PR tree-optimization/86401 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " \\+ 64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64 - " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\+ 4294967232" "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 319" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| 4294967168" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\^ 4294966912" "optimized" } } */

unsigned f1 (unsigned x) { unsigned m = 64; return (m + x) & (m - 1); }
unsigned f2 (unsigned x) { return (64 + x) & 63; }
unsigned f3 (unsigned x) { unsigned m = 64; return (x + m) & (m - 1); }
unsigned f4 (unsigned x) { return (x + 64) & 63; }
unsigned f5 (unsigned x) { unsigned m = 64; return (m - x) & (m - 1); }
unsigned f6 (unsigned x) { return (64 - x) & 63; }
unsigned f7 (unsigned x) { unsigned m = 64; return (x - m) & (m - 1); }
unsigned f8 (unsigned x) { return (x - 64) & 63; }
unsigned f9 (unsigned x, unsigned y) { unsigned m = 64, n = 256 | (m - 1); return ((x & n) + y) & (m - 1); }
unsigned f10 (unsigned x, unsigned y) { return ((x & 319) + y) & 63; }
unsigned f11 (unsigned x, unsigned y) { unsigned m = 64, n = -128; return ((x | n) + y) & (m - 1); }
unsigned f12 (unsigned x, unsigned y) { return ((x | -128) + y) & 63; }
unsigned f13 (unsigned x, unsigned y) { unsigned m = 64, n = -384; return ((x ^ n) + y) & (m - 1); }
unsigned f14 (unsigned x, unsigned y) { return ((x ^ -384) + y) & 63; }
unsigned f15 (unsigned x, unsigned y) { unsigned m = 64, n = 256 | (m - 1); return (y + (x & n)) & (m - 1); }
unsigned f16 (unsigned x, unsigned y) { return (y + (x & 319)) & 63; }
unsigned f17 (unsigned x, unsigned y) { unsigned m = 64, n = -128; return (y + (x | n)) & (m - 1); }
unsigned f18 (unsigned x, unsigned y) { return (y + (x | -128)) & 63; }
unsigned f19 (unsigned x, unsigned y) { unsigned m = 64, n = -384; return (y + (x ^ n)) & (m - 1); }
unsigned f20 (unsigned x, unsigned y) { return (y + (x ^ -384)) & 63; }
unsigned f21 (unsigned x, unsigned y) { unsigned m = 64, n = 256 | (m - 1); return ((x & n) - y) & (m - 1); }
unsigned f22 (unsigned x, unsigned y) { return ((x & 319) - y) & 63; }
unsigned f23 (unsigned x, unsigned y) { unsigned m = 64, n = -128; return ((x | n) - y) & (m - 1); }
unsigned f24 (unsigned x, unsigned y) { return ((x | -128) - y) & 63; }
unsigned f25 (unsigned x, unsigned y) { unsigned m = 64, n = -384; return ((x ^ n) - y) & (m - 1); }
unsigned f26 (unsigned x, unsigned y) { return ((x ^ -384) - y) & 63; }
unsigned f27 (unsigned x, unsigned y) { unsigned m = 64, n = 256 | (m - 1); return (y - (x & n)) & (m - 1); }
unsigned f28 (unsigned x, unsigned y) { return (y - (x & 319)) & 63; }
unsigned f29 (unsigned x, unsigned y) { unsigned m = 64, n = -128; return (y - (x | n)) & (m - 1); }
unsigned f30 (unsigned x, unsigned y) { return (y - (x | -128)) & 63; }
unsigned f31 (unsigned x, unsigned y) { unsigned m = 64, n = -384; return (y - (x ^ n)) & (m - 1); }
unsigned f32 (unsigned x, unsigned y) { return (y - (x ^ -384)) & 63; }
unsigned f33 (unsigned x) { unsigned m = 64, n = 256 | (m - 1); return (-(x & n)) & (m - 1); }
unsigned f34 (unsigned x) { return (-(x & 319)) & 63; }
unsigned f35 (unsigned x) { unsigned m = 64, n = -128; return (-(x | n)) & (m - 1); }
unsigned f36 (unsigned x) { return (-(x | -128)) & 63; }
unsigned f37 (unsigned x) { unsigned m = 64, n = -384; return (-(x ^ n)) & (m - 1); }
unsigned f38 (unsigned x) { return (-(x ^ -384)) & 63; }
