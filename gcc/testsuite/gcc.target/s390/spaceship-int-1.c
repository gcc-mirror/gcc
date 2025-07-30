/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mzarch -march=z13 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, -1\)} 4 optimized } } */
/* { dg-final { scan-tree-dump-times {\.SPACESHIP \([^,]+, [^,]+, 1\)} 5 optimized } } */
/* { dg-final { scan-assembler-times {\tlhi} 9 } } */
/* { dg-final { scan-assembler-times {\tloc} 18 } } */

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

TEST(signed char, schar)
TEST(unsigned char, uchar)
TEST(char, char)

TEST(short, sshort)
TEST(unsigned short, ushort)

TEST(int, sint)
TEST(unsigned int, uint)

TEST(long, slong)
TEST(unsigned long, ulong)
