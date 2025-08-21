/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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
    if (y <= 0)                                                                \
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

TEST_OP_CST (eq, ==, 1)
TEST_OP_CST (ne, !=, 2)
TEST_OP_CST (lt, <, 3)
TEST_OP_CST (gt, >, 4)
TEST_OP_CST (le, <=, 5)
TEST_OP_CST (ge, >=, 6)

TEST_OP (eq, ==)
TEST_OP (ne, !=)
TEST_OP (lt, <)
TEST_OP (gt, >)
TEST_OP (le, <=)
TEST_OP (ge, >=)

/* FIXME: The lt, le, gt and ge cases for int and enum don't get optimized.  */
/* { dg-final { scan-tree-dump-times "<<" 8 optimized } } */
