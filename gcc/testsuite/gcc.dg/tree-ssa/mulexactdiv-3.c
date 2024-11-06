/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

#define TEST_CMP(FN, TYPE1, DIV, TYPE2, MUL)	\
  TYPE2						\
  FN (TYPE1 x) 					\
  {						\
    if (x & 7)					\
      __builtin_unreachable ();			\
    x /= (TYPE1) (DIV);				\
    return (TYPE2) x * (TYPE2) (MUL);		\
  }

TEST_CMP (f1, int, 2, long, (~0UL >> 1) & -2)
TEST_CMP (f2, int, 4, unsigned long, -8)
TEST_CMP (f3, int, 8, unsigned int, -24)
TEST_CMP (f4, long, 2, int, (~0U >> 1) & -2)
TEST_CMP (f5, long, 4, unsigned int, 100)
TEST_CMP (f6, long, 8, unsigned long, 200)

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr, } "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr, } "optimized" } } */
