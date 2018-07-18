/* PR tree-optimization/81346 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 0;" 32 "optimized" } } */

int f00 (int x) { return x / 49152 > 49152; }
int f01 (int x) { int a = 49152, b = 49152; return x / a > b; }
int f02 (int x) { return x / 49152 >= 49152; }
int f03 (int x) { int a = 49152, b = 49152; return x / a >= b; }
int f04 (int x) { return x / 49152 < -49152; }
int f05 (int x) { int a = 49152, b = -49152; return x / a < b; }
int f06 (int x) { return x / 49152 <= -49152; }
int f07 (int x) { int a = 49152, b = -49152; return x / a <= b; }
int f08 (int x) { return x / 46340 > 46341; }
int f09 (int x) { int a = 46340, b = 46341; return x / a > b; }
int f10 (int x) { return x / 46340 < -46341; }
int f11 (int x) { int a = 46340, b = -46341; return x / a < b; }
int f12 (int x) { return x / 49152 == -49152; }
int f13 (int x) { int a = 49152, b = -49152; return x / a == b; }
int f14 (int x) { return x / 49152 == 49152; }
int f15 (int x) { int a = 49152, b = 49152; return x / a == b; }
int f16 (int x) { return x / -49152 > 49152; }
int f17 (int x) { int a = -49152, b = 49152; return x / a > b; }
int f18 (int x) { return x / -49152 >= 49152; }
int f19 (int x) { int a = -49152, b = 49152; return x / a >= b; }
int f20 (int x) { return x / -49152 < -49152; }
int f21 (int x) { int a = -49152, b = -49152; return x / a < b; }
int f22 (int x) { return x / -49152 <= -49152; }
int f23 (int x) { int a = -49152, b = -49152; return x / a <= b; }
int f24 (int x) { return x / -46340 > 46341; }
int f25 (int x) { int a = -46340, b = 46341; return x / a > b; }
int f26 (int x) { return x / -46340 < -46341; }
int f27 (int x) { int a = -46340, b = -46341; return x / a < b; }
int f28 (int x) { return x / -49152 == 49152; }
int f29 (int x) { int a = -49152, b = 49152; return x / a == b; }
int f30 (int x) { return x / -49152 == -49152; }
int f31 (int x) { int a = -49152, b = -49152; return x / a == b; }
