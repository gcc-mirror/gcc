/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-not "\tbl\t" } } */
/* { dg-final { scan-assembler-not "__aeabi" } } */
int x, y;

#define TEST_EXPR(NAME, ARGS, EXPR)			\
  int NAME##1 ARGS { return (EXPR); }			\
  int NAME##2 ARGS { return !(EXPR); }			\
  int NAME##3 ARGS { return (EXPR) ? x : y; }		\
  void NAME##4 ARGS { if (EXPR) x++; }			\
  void NAME##5 ARGS { if (!(EXPR)) x++; }

#define TEST(NAME, TYPE, OPERATOR) \
  TEST_EXPR (NAME##_rr, (TYPE a1, TYPE a2), a1 OPERATOR a2)	\
  TEST_EXPR (NAME##_rm, (TYPE a1, TYPE *a2), a1 OPERATOR *a2)	\
  TEST_EXPR (NAME##_mr, (TYPE *a1, TYPE a2), *a1 OPERATOR a2)	\
  TEST_EXPR (NAME##_mm, (TYPE *a1, TYPE *a2), *a1 OPERATOR *a2) \
  TEST_EXPR (NAME##_rc, (TYPE a1), a1 OPERATOR 100)		\
  TEST_EXPR (NAME##_cr, (TYPE a1), 100 OPERATOR a1)

#define TEST_OP(NAME, OPERATOR) \
  TEST (sc_##NAME, signed char, OPERATOR)		\
  TEST (uc_##NAME, unsigned char, OPERATOR)		\
  TEST (ss_##NAME, short, OPERATOR)			\
  TEST (us_##NAME, unsigned short, OPERATOR)		\
  TEST (si_##NAME, int, OPERATOR)			\
  TEST (ui_##NAME, unsigned int, OPERATOR)		\
  TEST (sll_##NAME, long long, OPERATOR)		\
  TEST (ull_##NAME, unsigned long long, OPERATOR)

TEST_OP (eq, ==)
TEST_OP (ne, !=)
TEST_OP (lt, <)
TEST_OP (gt, >)
TEST_OP (le, <=)
TEST_OP (ge, >=)
