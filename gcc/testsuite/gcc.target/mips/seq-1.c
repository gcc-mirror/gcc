/* { dg-do compile } */
/* { dg-final { scan-assembler-times "\tsltu\t|\tsltiu\t" 4 } } */

#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 int f##N (int a, int b) { return LHS REL RHS; }

TEST (0, a, ==, 0);
TEST (1, a, ==, 600);
TEST (10, a, !=, 0);
TEST (11, a, !=, -800);
