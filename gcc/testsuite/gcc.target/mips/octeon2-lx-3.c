/* { dg-do compile } */
/* { dg-options "-march=octeon2 -mgp32" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

#define TEST(N, T) \
  T f##N (T *p, int i) { return p[i]; } \
  unsigned T g##N (unsigned T *p, int i) { return p[i]; }

TEST (1, char)
/* { dg-final { scan-assembler-times "\tlbu?x\t" 2 } } */
TEST (2, short)
/* { dg-final { scan-assembler-times "\tlhu?x\t" 2 } } */
TEST (3, int)
/* { dg-final { scan-assembler-times "\tlwx\t" 2 } } */
