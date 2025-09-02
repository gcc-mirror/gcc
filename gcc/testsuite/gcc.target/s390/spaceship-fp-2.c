/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mzarch -march=z13 -ffinite-math-only -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, -128\)} 3 optimized } } */
/* { dg-final { scan-assembler-times {\tc[edx]br\t} 3 } } */
/* { dg-final { scan-assembler-not {\tbrc} } } */
/* { dg-final { scan-assembler-not {\tk[edx]br\t} } } */

#define TEST(T, U)		\
  int test_##U (T x, T y)	\
  {				\
    if (x == y)			\
      return 0;			\
    else if (x < y)		\
      return -1;		\
    else if (x > y)		\
      return 1;			\
    else			\
      return -128;		\
  }

TEST (float, float)
TEST (double, double)
TEST (long double, longdouble)
