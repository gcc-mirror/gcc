/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-require-effective-target c99_runtime } */

#include <stdbool.h>

extern void link_error(void);

#define T(FUNC1, FUNC2, CMP, TYPE, C_TY, ID)                       \
void test_##FUNC1##_##FUNC2##_##ID (TYPE x)                        \
{                                                                  \
  TYPE a = 10.0##C_TY;                                             \
  TYPE t1 = __builtin_##FUNC1(x);                                  \
  bool b1 = t1 CMP a;                                              \
  TYPE t2 = __builtin_##FUNC2(a);                                  \
  bool b2 = x CMP t2;                                              \
  if (b1 != b2)                                                    \
    link_error();                                                  \
}

#define TEST(FUNC1, FUNC2, TYPE, C_TY)                             \
  T(FUNC1, FUNC2, <=, TYPE, C_TY, 1)                               \
  T(FUNC1, FUNC2, >=, TYPE, C_TY, 2)                               \

#define TEST_ALL(TYPE, C_TY, F_TY)                                  \
  TEST(exp##F_TY, log##F_TY, TYPE, C_TY)                             \
  TEST(exp2##F_TY, log2##F_TY, TYPE, C_TY)                           \
  TEST(exp10##F_TY, log10##F_TY, TYPE, C_TY)                         \
  TEST(log##F_TY, exp##F_TY, TYPE, C_TY)                             \
  TEST(log2##F_TY, exp2##F_TY, TYPE, C_TY)                           \
  TEST(log10##F_TY, exp10##F_TY, TYPE, C_TY)

TEST_ALL(double, , )
TEST_ALL(float, f, f)
TEST_ALL(long double, L, l)

int main (void)
{
  return 0;
}
