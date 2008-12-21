/* { dg-options "(-mips16) -O -mabi=o64" } */

/* { dg-final { scan-assembler-not "and\t\|andi\t\|ext\t\|sll\t\|srl\t" } } */
/* { dg-final { scan-assembler-times "slt\t\|slti?u\t" 8 } } */


#define TEST(N, LHS, REL, RHS) \
  MIPS16 long long w##N (int a, int b) {return LHS REL RHS;} \
  MIPS16 int n##N (long long a, long long b) {return LHS REL RHS;} \

TEST (eq, a, ==, 0);

TEST (gt, a, >, b);

TEST (lt, a, <, b);
TEST (le, a, <=, 11);
