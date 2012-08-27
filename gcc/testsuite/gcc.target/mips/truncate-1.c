/* { dg-options "-mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

#define TEST(ID, TYPE, SHIFT)				\
  int __attribute__((nomips16))				\
  f##ID (unsigned long long y)				\
  {							\
    return (TYPE) ((TYPE) (y >> SHIFT) + 1);		\
  }

TEST (1, int, 32)
TEST (2, short, 32)
TEST (3, char, 32)
TEST (4, int, 33)
TEST (5, short, 33)
TEST (6, char, 33)
TEST (7, int, 61)
TEST (8, short, 61)
TEST (9, char, 61)

/* { dg-final { scan-assembler-not "\tsll\t\[^\n\]*,0" } } */
