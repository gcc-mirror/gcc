/* PR tree-optimization/81346 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " / " "optimized" } } */

__attribute__((noinline, noclone)) int f00 (int x) { return x / 46340 > -46341; }
__attribute__((noinline, noclone)) int f01 (int x) { int a = 46340, b = -46341; return x / a > b; }
__attribute__((noinline, noclone)) int f02 (int x) { return x / 46340 >= 46341; }
__attribute__((noinline, noclone)) int f03 (int x) { int a = 46340, b = 46341; return x / a >= b; }
__attribute__((noinline, noclone)) int f04 (int x) { return x / 46340 < 46341; }
__attribute__((noinline, noclone)) int f05 (int x) { int a = 46340, b = 46341; return x / a < b; }
__attribute__((noinline, noclone)) int f06 (int x) { return x / 46340 <= -46341; }
__attribute__((noinline, noclone)) int f07 (int x) { int a = 46340, b = -46341; return x / a <= b; }
__attribute__((noinline, noclone)) int f08 (int x) { return x / 46340 == -46341; }
__attribute__((noinline, noclone)) int f09 (int x) { int a = 46340, b = -46341; return x / a == b; }
__attribute__((noinline, noclone)) int f10 (int x) { return x / 46340 == 46341; }
__attribute__((noinline, noclone)) int f11 (int x) { int a = 46340, b = 46341; return x / a == b; }
__attribute__((noinline, noclone)) int f12 (int x) { return x / 46340 != -46341; }
__attribute__((noinline, noclone)) int f13 (int x) { int a = 46340, b = -46341; return x / a != b; }
__attribute__((noinline, noclone)) int f14 (int x) { return x / 46340 != 46341; }
__attribute__((noinline, noclone)) int f15 (int x) { int a = 46340, b = 46341; return x / a != b; }
__attribute__((noinline, noclone)) int f16 (int x) { return x / 15 > -15; }
__attribute__((noinline, noclone)) int f17 (int x) { int a = 15, b = -15; return x / a > b; }
__attribute__((noinline, noclone)) int f18 (int x) { return x / 15 > 15; }
__attribute__((noinline, noclone)) int f19 (int x) { int a = 15, b = 15; return x / a > b; }
__attribute__((noinline, noclone)) int f20 (int x) { return x / 15 >= -15; }
__attribute__((noinline, noclone)) int f21 (int x) { int a = 15, b = -15; return x / a >= b; }
__attribute__((noinline, noclone)) int f22 (int x) { return x / 15 >= 15; }
__attribute__((noinline, noclone)) int f23 (int x) { int a = 15, b = 15; return x / a >= b; }
__attribute__((noinline, noclone)) int f24 (int x) { return x / 15 < -15; }
__attribute__((noinline, noclone)) int f25 (int x) { int a = 15, b = -15; return x / a < b; }
__attribute__((noinline, noclone)) int f26 (int x) { return x / 15 < 15; }
__attribute__((noinline, noclone)) int f27 (int x) { int a = 15, b = 15; return x / a < b; }
__attribute__((noinline, noclone)) int f28 (int x) { return x / 15 <= -15; }
__attribute__((noinline, noclone)) int f29 (int x) { int a = 15, b = -15; return x / a <= b; }
__attribute__((noinline, noclone)) int f30 (int x) { return x / 15 <= 15; }
__attribute__((noinline, noclone)) int f31 (int x) { int a = 15, b = 15; return x / a <= b; }
__attribute__((noinline, noclone)) int f32 (int x) { return x / 15 == -15; }
__attribute__((noinline, noclone)) int f33 (int x) { int a = 15, b = -15; return x / a == b; }
__attribute__((noinline, noclone)) int f34 (int x) { return x / 15 == 15; }
__attribute__((noinline, noclone)) int f35 (int x) { int a = 15, b = 15; return x / a == b; }
__attribute__((noinline, noclone)) int f36 (int x) { return x / 15 != -15; }
__attribute__((noinline, noclone)) int f37 (int x) { int a = 15, b = -15; return x / a != b; }
__attribute__((noinline, noclone)) int f38 (int x) { return x / 15 != 15; }
__attribute__((noinline, noclone)) int f39 (int x) { int a = 15, b = 15; return x / a != b; }
__attribute__((noinline, noclone)) int f40 (int x) { return x / -46340 > -46341; }
__attribute__((noinline, noclone)) int f41 (int x) { int a = -46340, b = -46341; return x / a > b; }
__attribute__((noinline, noclone)) int f42 (int x) { return x / -46340 >= 46341; }
__attribute__((noinline, noclone)) int f43 (int x) { int a = -46340, b = 46341; return x / a >= b; }
__attribute__((noinline, noclone)) int f44 (int x) { return x / -46340 < 46341; }
__attribute__((noinline, noclone)) int f45 (int x) { int a = -46340, b = 46341; return x / a < b; }
__attribute__((noinline, noclone)) int f46 (int x) { return x / -46340 <= -46341; }
__attribute__((noinline, noclone)) int f47 (int x) { int a = -46340, b = -46341; return x / a <= b; }
__attribute__((noinline, noclone)) int f48 (int x) { return x / -46340 == 46341; }
__attribute__((noinline, noclone)) int f49 (int x) { int a = -46340, b = 46341; return x / a == b; }
__attribute__((noinline, noclone)) int f50 (int x) { return x / -46340 == -46341; }
__attribute__((noinline, noclone)) int f51 (int x) { int a = -46340, b = -46341; return x / a == b; }
__attribute__((noinline, noclone)) int f52 (int x) { return x / -46340 != 46341; }
__attribute__((noinline, noclone)) int f53 (int x) { int a = -46340, b = 46341; return x / a != b; }
__attribute__((noinline, noclone)) int f54 (int x) { return x / -46340 != -46341; }
__attribute__((noinline, noclone)) int f55 (int x) { int a = -46340, b = -46341; return x / a != b; }
__attribute__((noinline, noclone)) int f56 (int x) { return x / -15 > 15; }
__attribute__((noinline, noclone)) int f57 (int x) { int a = -15, b = 15; return x / a > b; }
__attribute__((noinline, noclone)) int f58 (int x) { return x / -15 > -15; }
__attribute__((noinline, noclone)) int f59 (int x) { int a = -15, b = -15; return x / a > b; }
__attribute__((noinline, noclone)) int f60 (int x) { return x / -15 >= 15; }
__attribute__((noinline, noclone)) int f61 (int x) { int a = -15, b = 15; return x / a >= b; }
__attribute__((noinline, noclone)) int f62 (int x) { return x / -15 >= -15; }
__attribute__((noinline, noclone)) int f63 (int x) { int a = -15, b = -15; return x / a >= b; }
__attribute__((noinline, noclone)) int f64 (int x) { return x / -15 < 15; }
__attribute__((noinline, noclone)) int f65 (int x) { int a = -15, b = 15; return x / a < b; }
__attribute__((noinline, noclone)) int f66 (int x) { return x / -15 < -15; }
__attribute__((noinline, noclone)) int f67 (int x) { int a = -15, b = -15; return x / a < b; }
__attribute__((noinline, noclone)) int f68 (int x) { return x / -15 <= 15; }
__attribute__((noinline, noclone)) int f69 (int x) { int a = -15, b = 15; return x / a <= b; }
__attribute__((noinline, noclone)) int f70 (int x) { return x / -15 <= -15; }
__attribute__((noinline, noclone)) int f71 (int x) { int a = -15, b = -15; return x / a <= b; }
__attribute__((noinline, noclone)) int f72 (int x) { return x / -15 == 15; }
__attribute__((noinline, noclone)) int f73 (int x) { int a = -15, b = 15; return x / a == b; }
__attribute__((noinline, noclone)) int f74 (int x) { return x / -15 == -15; }
__attribute__((noinline, noclone)) int f75 (int x) { int a = -15, b = -15; return x / a == b; }
__attribute__((noinline, noclone)) int f76 (int x) { return x / -15 != 15; }
__attribute__((noinline, noclone)) int f77 (int x) { int a = -15, b = 15; return x / a != b; }
__attribute__((noinline, noclone)) int f78 (int x) { return x / -15 != -15; }
__attribute__((noinline, noclone)) int f79 (int x) { int a = -15, b = -15; return x / a != b; }
__attribute__((noinline, noclone)) int f80 (int x) { return x / -15 > 0; }
__attribute__((noinline, noclone)) int f81 (int x) { int a = -15, b = 0; return x / a > b; }
__attribute__((noinline, noclone)) int f82 (int x) { return x / 15 > 0; }
__attribute__((noinline, noclone)) int f83 (int x) { int a = 15, b = 0; return x / a > b; }
__attribute__((noinline, noclone)) int f84 (int x) { return x / -15 >= 0; }
__attribute__((noinline, noclone)) int f85 (int x) { int a = -15, b = 0; return x / a >= b; }
__attribute__((noinline, noclone)) int f86 (int x) { return x / 15 >= 0; }
__attribute__((noinline, noclone)) int f87 (int x) { int a = 15, b = 0; return x / a >= b; }
__attribute__((noinline, noclone)) int f88 (int x) { return x / -15 < 0; }
__attribute__((noinline, noclone)) int f89 (int x) { int a = -15, b = 0; return x / a < b; }
__attribute__((noinline, noclone)) int f90 (int x) { return x / 15 < 0; }
__attribute__((noinline, noclone)) int f91 (int x) { int a = 15, b = 0; return x / a < b; }
__attribute__((noinline, noclone)) int f92 (int x) { return x / -15 <= 0; }
__attribute__((noinline, noclone)) int f93 (int x) { int a = -15, b = 0; return x / a <= b; }
__attribute__((noinline, noclone)) int f94 (int x) { return x / 15 <= 0; }
__attribute__((noinline, noclone)) int f95 (int x) { int a = 15, b = 0; return x / a <= b; }
__attribute__((noinline, noclone)) int f96 (int x) { return x / -15 == 0; }
__attribute__((noinline, noclone)) int f97 (int x) { int a = -15, b = 0; return x / a == b; }
__attribute__((noinline, noclone)) int f98 (int x) { return x / 15 == 0; }
__attribute__((noinline, noclone)) int f99 (int x) { int a = 15, b = 0; return x / a == b; }
__attribute__((noinline, noclone)) int f100 (int x) { return x / -15 != 0; }
__attribute__((noinline, noclone)) int f101 (int x) { int a = -15, b = 0; return x / a != b; }
__attribute__((noinline, noclone)) int f102 (int x) { return x / 15 != 0; }
__attribute__((noinline, noclone)) int f103 (int x) { int a = 15, b = 0; return x / a != b; }
