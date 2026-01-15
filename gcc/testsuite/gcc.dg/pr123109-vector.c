/* { dg-do compile } */
/* { dg-options "-Wno-psabi -O2 -fdump-tree-forwprop2" } */
/* { dg-additional-options "-msse2" { target { i?86-*-* x86_64-*-* } } } */

typedef int v4si __attribute__((vector_size(4 * sizeof(int))));
typedef unsigned int v4usi __attribute__((vector_size(4 * sizeof(unsigned int))));

#define TEST_NE(type)                                         \
  type test_ne_##type (type a) { return (a >> 31) != 0; }

#define TEST_EQ(type)                                         \
  type test_eq_##type (type a) { return (a >> 31) == 0; }

TEST_NE(v4si)
TEST_NE(v4usi)
TEST_EQ(v4si)
TEST_EQ(v4usi)

/* { dg-final { scan-tree-dump-times ">= { 0, 0, 0, 0 }" 2 forwprop2 } } */
/* "a < 0 ? -1 : 0" will be optimized to "a >> 31" only if there's an optab.  */
/* { dg-final { scan-tree-dump-times ">> 31" 2 forwprop2 { target { i?86-*-* x86_64-*-* aarch64-*-* } } } } */
