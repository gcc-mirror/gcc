/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define TEST_NE(type)                                         \
  type test_ne_##type (type a) { return (a >> 31) != 0; }

#define TEST_EQ(type)                                         \
  type test_eq_##type (type a) { return (a >> 31) == 0; }

TEST_NE(int)
TEST_NE(unsigned)
TEST_EQ(int)
TEST_EQ(unsigned)

/* { dg-final { scan-tree-dump-times ">= 0" 2 optimized } } */
/* { dg-final { scan-tree-dump-times "< 0" 2 optimized } } */

