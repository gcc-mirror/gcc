/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mzarch -march=z13 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, -1\)} 1 optimized } } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, 1\)} 1 optimized } } */
/* { dg-final { scan-assembler-times {\tvecg} 1 } } */
/* { dg-final { scan-assembler-times {\tveclg} 1 } } */
/* { dg-final { scan-assembler-times {\tvchlgs} 2 } } */
/* { dg-final { scan-assembler-times {\tvceqgs} 2 } } */
/* { dg-final { scan-assembler-times {\tlhi} 2 } } */
/* { dg-final { scan-assembler-times {\tloc} 4 } } */

#define TEST(T, U)		\
  int test_##U (T x, T y)	\
  {				\
    if (x == y)			\
      return 0;			\
    else if (x < y)		\
      return -1;		\
    else			\
      return 1;			\
  }

TEST(__int128, sint128)
TEST(unsigned __int128, uint128)
