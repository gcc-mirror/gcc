/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

#define TEST_CMP(FN, DIV, ADD, MUL)		\
  int						\
  FN (int x)   					\
  {						\
    if (x & 7)					\
      __builtin_unreachable ();			\
    x /= DIV;					\
    x += ADD;					\
    return x * MUL;				\
  }

TEST_CMP (f1, 2, 1, 3)
TEST_CMP (f2, 4, 2, 2)
TEST_CMP (f3, 4, 3, 6)
TEST_CMP (f4, 8, 4, 2)
TEST_CMP (f5, 8, 5, 4)

/* { dg-final { scan-tree-dump-times {<[a-z]*_div_expr,} 5 "optimized" } } */
