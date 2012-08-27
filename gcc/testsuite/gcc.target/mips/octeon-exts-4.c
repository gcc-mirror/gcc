/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tsll\t\[^\n\]*,0" } } */
/* { dg-final { scan-assembler-times "\texts\t" 6 } } */

#define TEST(ID, TYPE, SHIFT)				\
  int NOMIPS16						\
  f##ID (long long y)					\
  {							\
    return (TYPE) ((TYPE) (y >> SHIFT) + 1);		\
  }							\
  int NOMIPS16						\
  g##ID (unsigned long long y)				\
  {							\
    return (TYPE) ((TYPE) (y >> SHIFT) + 1);		\
  }

TEST (1, int, 10)
TEST (2, short, 5)
TEST (3, char, 31)
