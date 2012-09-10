/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* { dg-final { scan-assembler-not "and\t\|andi\t\|ext\t\|sll\t\|srl\t" } } */
/* { dg-final { scan-assembler-times "\tseqi\t\|\tsnei\t" 4 } } */


#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 long long w##N (int a, int b) {return LHS REL RHS;} \
  NOMIPS16 int n##N (long long a, long long b) {return LHS REL RHS;} \

TEST (eq, a, ==, 10);
TEST (ne, a, !=, 32);
