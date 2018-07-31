/* { dg-do compile }
/* { dg-options "-msse2 -O2 -ftree-vectorize" } */
/* { dg-final { scan-assembler "pavgb" } } */
/* { dg-final { scan-assembler "pavgw" } } */

#define N 1024

#define TEST(TYPE)						\
  unsigned TYPE a_##TYPE[N], b_##TYPE[N], c_##TYPE[N];		\
  void f_##TYPE (void)						\
  {								\
    int i;							\
    for (i = 0; i < N; i++)					\
      a_##TYPE[i] = (b_##TYPE[i] + c_##TYPE[i] + 1) >> 1;	\
  }

TEST(char);
TEST(short);
