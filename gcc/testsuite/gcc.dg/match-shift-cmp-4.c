/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* The fold (y << x) <op> x -> 0|1 should trigger when y is negative
   unsigned.  */

#define TEST_ONE_CST(n, op, type, cst)                                         \
  bool lshift_cst_##type##_##n (type x) { return ((unsigned) (cst) << x) op x; }

#define TEST_OP_CST(n, op, cst)                                                \
  TEST_ONE_CST (n, op, unsigned, cst)                                          \
  TEST_ONE_CST (n, op, int, cst)                                               \
  TEST_ONE_CST (n, op, test_enum, cst)

#define TEST_ONE(n, op, type)                                                  \
  bool lshift_##type##_##n (type x, type y)                                    \
  {                                                                            \
    if ((int) y <= 0)                                                          \
      __builtin_unreachable ();                                                \
    return ((unsigned) (y) << x) op x;                                         \
  }

#define TEST_OP(n, op)                                                         \
  TEST_ONE (n, op, unsigned)                                                   \
  TEST_ONE (n, op, int)                                                        \
  TEST_ONE (n, op, test_enum)

typedef enum
{
  MONE = -1,
  ZERO = 0,
  ONE = 1,
  TWO = 2
} test_enum;

TEST_OP_CST (eq, ==, -1)
TEST_OP_CST (ne, !=, -2)
TEST_OP_CST (lt, <, -3)
TEST_OP_CST (gt, >, -4)
TEST_OP_CST (le, <=, -5)
TEST_OP_CST (ge, >=, -6)

TEST_OP (eq, ==)
TEST_OP (ne, !=)
TEST_OP (lt, <)
TEST_OP (gt, >)
TEST_OP (le, <=)
TEST_OP (ge, >=)

/* { dg-final { scan-tree-dump-times "return 0;" 18 optimized } } */
/* { dg-final { scan-tree-dump-times "return 1;" 18 optimized } } */
