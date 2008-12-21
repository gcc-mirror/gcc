/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-final { scan-assembler-times "\tseq\t|\tseqi\t" 3 } } */
/* { dg-final { scan-assembler-times "\tsne\t|\tsnei\t" 3 } } */

#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 long long f##N (long long a, long long b) { return LHS REL RHS; }

TEST (0, a, ==, b);
TEST (1, a, ==, 23);
TEST (2, a, ==, 511);

TEST (3, a, !=, b);
TEST (4, a, !=, 1);
TEST (5, a, !=, 350);
