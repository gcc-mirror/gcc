/* PR tree-optimization/94589 */
/* { dg-do compile { target inf } } */
/* { dg-options "-O2 -g0 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\[ij]_\[0-9]+\\(D\\) (?:<|<=|>|>=) \[ij]_\[0-9]+\\(D\\)" 24 "optimized" } } */
/* { dg-final { scan-tree-dump-times "i_\[0-9]+\\(D\\) (?:<|<=|>|>=) 5\\.0" 24 "optimized" } } */

#define A __attribute__((noipa))
A int f3 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c > 0; }
A int f4 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c < 0; }
A int f5 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c >= 0; }
A int f6 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c <= 0; }
A int f7 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c == -1; }
A int f8 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c != -1; }
A int f9 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c > -1; }
A int f10 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c <= -1; }
A int f11 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c == 1; }
A int f12 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c != 1; }
A int f13 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c < 1; }
A int f14 (double i, double j) { int c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; return c >= 1; }
A int f17 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c > 0; }
A int f18 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c < 0; }
A int f19 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c >= 0; }
A int f20 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c <= 0; }
A int f21 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c == -1; }
A int f22 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c != -1; }
A int f23 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c > -1; }
A int f24 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c <= -1; }
A int f25 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c == 1; }
A int f26 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c != 1; }
A int f27 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c < 1; }
A int f28 (double i) { int c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; return c >= 1; }
A signed char f31 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f < 0; }
A signed char f32 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f > 0; }
A signed char f33 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f <= 0; }
A signed char f34 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f >= 0; }
A signed char f35 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f == 1; }
A signed char f36 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f != 1; }
A signed char f37 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f < 1; }
A signed char f38 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f >= 1; }
A signed char f39 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f == -1; }
A signed char f40 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f != -1; }
A signed char f41 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f > -1; }
A signed char f42 (double i, double j) { signed char c; if (i == j) c = 0; else if (i < j) c = -1; else if (i > j) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f <= -1; }
A signed char f45 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f < 0; }
A signed char f46 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f > 0; }
A signed char f47 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f <= 0; }
A signed char f48 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f >= 0; }
A signed char f49 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f == 1; }
A signed char f50 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f != 1; }
A signed char f51 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f < 1; }
A signed char f52 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f >= 1; }
A signed char f53 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f == -1; }
A signed char f54 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f != -1; }
A signed char f55 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f > -1; }
A signed char f56 (double i) { signed char c; if (i == 5.0) c = 0; else if (i < 5.0) c = -1; else if (i > 5.0) c = 1; else c = -128; unsigned char d = c; unsigned char e = -d; signed char f = e; return f <= -1; }
