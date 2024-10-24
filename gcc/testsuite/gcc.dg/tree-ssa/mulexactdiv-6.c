/* { dg-options "-O2 -fwrapv -fdump-tree-optimized-raw" } */

#define TEST_CMP(FN, DIV, ADD, MUL)		\
  int						\
  FN (int x)   					\
  {						\
    if (x & 7)					\
      __builtin_unreachable ();			\
    x /= DIV;					\
    x += (int) (ADD);				\
    return x * MUL;				\
  }

TEST_CMP (f1, 2, ~0U >> 1, 6)
TEST_CMP (f2, 2, ~(~0U >> 2), 10)

void
cmp1 (int x)
{
  if (x & 3)
    __builtin_unreachable ();

  int y = x / 4;
  y += (int) (~0U / 3U);
  y *= 8;

  unsigned z = x;
  z *= 2U;
  z += ~0U / 3U * 8U;

  if (y != (int) z)
    __builtin_abort ();
}


void
cmp2 (int x)
{
  if (x & 63)
    __builtin_unreachable ();

  unsigned y = x / 64;
  y += 100U;
  int y2 = (int) y * 256;

  unsigned z = x;
  z *= 4U;
  z += 25600U;

  if (y2 != (int) z)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not {<[a-z]*_div_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {<rshift_expr,} "optimized" } } */
/* { dg-final { scan-tree-dump-not {gimple_call <} "optimized" } } */
/* { dg-final { scan-tree-dump-times {<nop_expr,} 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<mult_expr,} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {<plus_expr,} 2 "optimized" } } */
