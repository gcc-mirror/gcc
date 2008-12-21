/* Check if we expand seq and sne.  */

/* { dg-do compile } */
/* { dg-options "-march=octeon" } */
/* { dg-final { scan-assembler-times "\tseq\t|\tseqi\t" 4 } } */
/* { dg-final { scan-assembler-times "\tsne\t|\tsnei\t" 4 } } */

#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 int f##N (int a, int b) { return LHS REL RHS; }

TEST (0, a, ==, b);
TEST (1, a, ==, 23);
TEST (2, a, ==, 511);
TEST (3, a, ==, -200);

TEST (10, a, !=, b);
TEST (11, a, !=, 1);
TEST (12, a, !=, 350);
TEST (13, a, !=, -512);
