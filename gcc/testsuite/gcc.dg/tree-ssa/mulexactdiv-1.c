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

TEST_CMP (f1, 2, 6)
TEST_CMP (f2, 2, 10)
TEST_CMP (f3, 4, 80)
TEST_CMP (f4, 8, 200)

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr, } "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr, } "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 3,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 5,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 20,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 25,} "optimized" } } */
