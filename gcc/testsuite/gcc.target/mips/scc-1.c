/* { dg-options "(-mips16) -O isa_rev>=1" } */

/* { dg-final { scan-assembler-times {slt	\$2,\$5,\$4} 1 } } */
/* { dg-final { scan-assembler-times {sltu	\$2,\$5,\$4} 1 } } */
/* { dg-final { scan-assembler-times {slt	\$5,\$4} 1 } } */
/* { dg-final { scan-assembler-times {sltu	\$5,\$4} 1 } } */

/* { dg-final { scan-assembler-times {slt	\$2,\$0,\$4} 1 } } */
/* { dg-final { scan-assembler-times {sltu	\$2,\$0,\$4} 1 } } */

/* { dg-final { scan-assembler-times {slt	\$2,\$4,\$5} 1 } } */
/* { dg-final { scan-assembler-times {sltu	\$2,\$4,\$5} 1 } } */
/* { dg-final { scan-assembler-times {slt	\$4,\$5} 1 } } */
/* { dg-final { scan-assembler-times {sltu	\$4,\$5} 1 } } */

/* { dg-final { scan-assembler-times {slt	\$2,\$4,23}  1 } } */
/* { dg-final { scan-assembler-times {sltu	\$2,\$4,23}  1 } } */
/* { dg-final { scan-assembler-times {slt	\$4,23}  1 } } */
/* { dg-final { scan-assembler-times {sltu	\$4,23}  1 } } */

#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 int s##N (int a, int b) { return LHS REL RHS; } \
  NOMIPS16 int u##N (unsigned a, unsigned b) { return LHS REL RHS; } \
  MIPS16 int s##N##_16 (int a, int b) { return LHS REL RHS; } \
  MIPS16 int u##N##_16 (unsigned a, unsigned b) { return LHS REL RHS; }

#define TEST_NO16(N, LHS, REL, RHS) \
  NOMIPS16 int s##N (int a, int b) { return LHS REL RHS; } \
  NOMIPS16 int u##N (unsigned a, unsigned b) { return LHS REL RHS; }

TEST (1, a, >, b);
TEST_NO16 (2, a, >=, 1);
TEST (3, a, <, b);
TEST (4, a, <=, 22);
