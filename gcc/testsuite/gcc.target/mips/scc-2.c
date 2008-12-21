/* { dg-do compile } */
/* { dg-options "-O -mgp64" } */

/* { dg-final { scan-assembler-not "and\t\|andi\t\|ext\t\|sll\t\|srl\t" } } */
/* { dg-final { scan-assembler-times "slt\t\|slti?u\t" 12 } } */


#define TEST(N, LHS, REL, RHS) \
  NOMIPS16 long long w##N (int a, int b) {return LHS REL RHS;} \
  NOMIPS16 int n##N (long long a, long long b) {return LHS REL RHS;} \

TEST (eq, a, ==, 0);
TEST (ne, a, !=, 0);
TEST (gt, a, >, b);
TEST (ge, a, >=, 1);
TEST (lt, a, <, b);
TEST (le, a, <=, 11);
