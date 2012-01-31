/* { dg-do compile } */
/* { dg-options "-march=octeon2 -O -mgp64" } */

#define TEST(N, R, T) \
 T fll##N (T j, signed R *b, long long i) { return j + b[i]; } \
 T gll##N (T j, unsigned R *b, long long i) { return j + b[i]; } \
 T fi##N (T j, signed R *b, int i) { return j + b[i]; } \
 T gi##N (T j, unsigned R *b, int i) { return j + b[i]; } \

TEST (1, char, int)
TEST (2, char, long long)
/* { dg-final { scan-assembler-times "\tlbx\t" 4 } } */
/* { dg-final { scan-assembler-times "\tlbux\t" 4 } } */
TEST (3, short, int)
TEST (4, short, long long)
/* { dg-final { scan-assembler-times "\tlhx\t" 4 } } */
/* { dg-final { scan-assembler-times "\tlhux\t" 4 } } */
TEST (5, int, long long)
/* { dg-final { scan-assembler-times "\tlwx\t" 2 } } */
/* { dg-final { scan-assembler-times "\tlwux\t" 2 } } */
