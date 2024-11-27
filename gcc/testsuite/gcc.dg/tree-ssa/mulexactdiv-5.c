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

TEST_CMP (f1, 2, 1, 6)
TEST_CMP (f2, 2, 2, 10)
TEST_CMP (f3, 4, 3, 80)
TEST_CMP (f4, 8, 4, 200)

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-times {<nop_expr,} 8 "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 3,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 5,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 20,} "optimized" } } */
/* { dg-final { scan-tree-dump {<mult_expr, [^,]*, [^,]*, 25,} "optimized" } } */
/* { dg-final { scan-tree-dump {<plus_expr, [^,]*, [^,]*, 6,} "optimized" } } */
/* { dg-final { scan-tree-dump {<plus_expr, [^,]*, [^,]*, 20,} "optimized" } } */
/* { dg-final { scan-tree-dump {<plus_expr, [^,]*, [^,]*, 240,} "optimized" } } */
/* { dg-final { scan-tree-dump {<plus_expr, [^,]*, [^,]*, 800,} "optimized" } } */
