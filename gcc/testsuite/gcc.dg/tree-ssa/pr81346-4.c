/* PR tree-optimization/81346 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "pr81346-3.c"

#define INT_MAX __INT_MAX__
#define INT_MIN (-__INT_MAX__ - 1)

extern void abort (void);

int
main ()
{
  if (__CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4)
    return 0;
#define TEST(fn1, fn2, v1, v2) \
  do { \
    int w1 = v1; int w2 = v2; \
    int in = 1; if (w1 > w2) { in = w1; w1 = w2; w2 = in; in = 0; } \
    if (w1 != INT_MIN) { if (fn1 (w1 - 1) != !in || fn2 (w1 - 1) != !in) abort (); } \
    if (fn1 (w1) != in || fn2 (w1) != in) abort (); \
    if (fn1 (w2) != in || fn2 (w2) != in) abort (); \
    if (w2 != INT_MAX) { if (fn1 (w2 + 1) != !in || fn2 (w2 + 1) != !in) abort (); } \
  } while (0)
TEST (f00, f01, -2147441939, INT_MAX);
TEST (f02, f03, 2147441940, INT_MAX);
TEST (f04, f05, INT_MIN, 2147441939);
TEST (f06, f07, INT_MIN, -2147441940);
TEST (f08, f09, INT_MIN, -2147441940);
TEST (f10, f11, 2147441940, INT_MAX);
TEST (f12, f13, -2147441939, INT_MAX);
TEST (f14, f15, INT_MIN, 2147441939);
TEST (f16, f17, -224, INT_MAX);
TEST (f18, f19, 240, INT_MAX);
TEST (f20, f21, -239, INT_MAX);
TEST (f22, f23, 225, INT_MAX);
TEST (f24, f25, INT_MIN, -240);
TEST (f26, f27, INT_MIN, 224);
TEST (f28, f29, INT_MIN, -225);
TEST (f30, f31, INT_MIN, 239);
TEST (f32, f33, -239, -225);
TEST (f34, f35, 225, 239);
TEST (f36, f37, -225, -239);
TEST (f38, f39, 239, 225);
TEST (f40, f41, INT_MIN, 2147441939);
TEST (f42, f43, INT_MIN, -2147441940);
TEST (f44, f45, -2147441939, INT_MAX);
TEST (f46, f47, 2147441940, INT_MAX);
TEST (f48, f49, INT_MIN, -2147441940);
TEST (f50, f51, 2147441940, INT_MAX);
TEST (f52, f53, -2147441939, INT_MAX);
TEST (f54, f55, INT_MIN, 2147441939);
TEST (f56, f57, INT_MIN, -240);
TEST (f58, f59, INT_MIN, 224);
TEST (f60, f61, INT_MIN, -225);
TEST (f62, f63, INT_MIN, 239);
TEST (f64, f65, -224, INT_MAX);
TEST (f66, f67, 240, INT_MAX);
TEST (f68, f69, -239, INT_MAX);
TEST (f70, f71, 225, INT_MAX);
TEST (f72, f73, -239, -225);
TEST (f74, f75, 225, 239);
TEST (f76, f77, -225, -239);
TEST (f78, f79, 239, 225);
TEST (f80, f81, INT_MIN, -15);
TEST (f82, f83, 15, INT_MAX);
TEST (f84, f85, INT_MIN, 14);
TEST (f86, f87, -14, INT_MAX);
TEST (f88, f89, 15, INT_MAX);
TEST (f90, f91, INT_MIN, -15);
TEST (f92, f93, -14, INT_MAX);
TEST (f94, f95, INT_MIN, 14);
TEST (f96, f97, -14, 14);
TEST (f98, f99, -14, 14);
TEST (f100, f101, 14, -14);
TEST (f102, f103, 14, -14);
  return 0;
}
