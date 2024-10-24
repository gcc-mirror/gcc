/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

#define TEST_CMP(FN, DIV, MUL)			\
  int						\
  FN (int x)   					\
  {						\
    if (x & 7)					\
      __builtin_unreachable ();			\
    x /= DIV;					\
    return x * MUL;				\
  }

TEST_CMP (f1, 2, 1)
TEST_CMP (f2, 2, 5)
TEST_CMP (f3, 4, 10)
TEST_CMP (f4, 8, 100)
TEST_CMP (f5, 16, 32)

/* { dg-final { scan-tree-dump-times {<[a-z]*_div_expr, } 5 "optimized" } } */
