/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* The fold (y << x) <op> x -> 0|1 shouldn't trigger when y is 0.  */

#define TEST_ONE_CST(n, op, type, cst)                                         \
  bool lshift_cst_##type##_##n (type x) { return (cst << x) op x; }

#define TEST_OP_CST(n, op, cst)                                                \
  TEST_ONE_CST (n, op, unsigned, cst)                                          \
  TEST_ONE_CST (n, op, int, cst)                                               \
  TEST_ONE_CST (n, op, bool, cst)                                              \
  TEST_ONE_CST (n, op, test_enum, cst)

#define TEST_ONE(n, op, type)                                                  \
  bool lshift_##type##_##n (type x, type y)                                    \
  {                                                                            \
    if (y != 0)                                                                \
      __builtin_unreachable ();                                                \
    return (y << x) op x;                                                      \
  }

#define TEST_OP(n, op)                                                         \
  TEST_ONE (n, op, unsigned)                                                   \
  TEST_ONE (n, op, int)                                                        \
  TEST_ONE (n, op, bool)                                                       \
  TEST_ONE (n, op, test_enum)

typedef enum
{
  MONE = -1,
  ZERO = 0,
  ONE = 1,
  TWO = 2
} test_enum;

TEST_OP_CST (eq, ==, 0)
TEST_OP_CST (ne, !=, 0)

TEST_OP (eq, ==)
TEST_OP (ne, !=)

/* These end up getting folded by other patterns.  */
/* { dg-final { scan-tree-dump-times "x_\\d\\(D\\) == 0" 6 optimized } } */
/* { dg-final { scan-tree-dump-times "x_\\d\\(D\\) != 0" 6 optimized } } */
/* { dg-final { scan-tree-dump-times "~x_\\d\\(D\\)" 2 optimized } } */
/* { dg-final { scan-tree-dump-times "return x_\\d\\(D\\);" 2 optimized } } */
