/* { dg-do compile { target { x86_64-*-* aarch64-*-* } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef int v4si __attribute__((vector_size(4 * sizeof(int))));
typedef unsigned int v4usi __attribute__((vector_size(4 * sizeof(unsigned int))));

#define TEST_NE(type)                                         \
  type test_ne_##type (type a) { return (a >> 31) != 0; }

#define TEST_EQ(type)                                         \
  type test_eq_##type (type a) { return (a >> 31) == 0; }

TEST_NE(int)
TEST_NE(unsigned)
TEST_NE(v4si)
TEST_NE(v4usi)
TEST_EQ(int)
TEST_EQ(unsigned)
TEST_EQ(v4si)
TEST_EQ(v4usi)

/* { dg-final { scan-tree-dump-times ">= 0" 2 optimized } } */
/* { dg-final { scan-tree-dump-times "< 0" 2 optimized } } */
/* { dg-final { scan-tree-dump-times ">= { 0, 0, 0, 0 }" 2 optimized } } */
/* { dg-final { scan-tree-dump-times ">> 31" 2 optimized } } */
