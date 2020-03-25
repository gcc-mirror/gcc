/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-O" } */
/* { dg-add-options arm_fp } */
/* { dg-final { scan-assembler-not "\tbl\t" } } */
/* { dg-final { scan-assembler-not "__aeabi" } } */
int x, y;

#define EQ(X, Y) ((X) == (Y))
#define NE(X, Y) ((X) != (Y))
#define LT(X, Y) ((X) < (Y))
#define GT(X, Y) ((X) > (Y))
#define LE(X, Y) ((X) <= (Y))
#define GE(X, Y) ((X) >= (Y))

#define TEST_EXPR(NAME, ARGS, EXPR)			\
  int NAME##1 ARGS { return (EXPR); }			\
  int NAME##2 ARGS { return !(EXPR); }			\
  int NAME##3 ARGS { return (EXPR) ? x : y; }		\
  void NAME##4 ARGS { if (EXPR) x++; }			\
  void NAME##5 ARGS { if (!(EXPR)) x++; }

#define TEST(NAME, TYPE, OPERATOR) \
  TEST_EXPR (NAME##_rr, (TYPE a1, TYPE a2), OPERATOR (a1, a2))		\
  TEST_EXPR (NAME##_rm, (TYPE a1, TYPE *a2), OPERATOR (a1, *a2))	\
  TEST_EXPR (NAME##_mr, (TYPE *a1, TYPE a2), OPERATOR (*a1, a2))	\
  TEST_EXPR (NAME##_mm, (TYPE *a1, TYPE *a2), OPERATOR (*a1, *a2))	\
  TEST_EXPR (NAME##_rc, (TYPE a1), OPERATOR (a1, 100))			\
  TEST_EXPR (NAME##_cr, (TYPE a1), OPERATOR (100, a1))

#define TEST_OP(NAME, OPERATOR) \
  TEST (f_##NAME, float, OPERATOR)

TEST_OP (eq, EQ)
TEST_OP (ne, NE)
TEST_OP (lt, LT)
TEST_OP (gt, GT)
TEST_OP (le, LE)
TEST_OP (ge, GE)
TEST_OP (blt, __builtin_isless)
TEST_OP (bgt, __builtin_isgreater)
TEST_OP (ble, __builtin_islessequal)
TEST_OP (bge, __builtin_isgreaterequal)
/* This one should be expanded into separate ordered and equality
   comparisons.  */
TEST_OP (blg, __builtin_islessgreater)
TEST_OP (bun, __builtin_isunordered)
