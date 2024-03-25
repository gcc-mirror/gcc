/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */

#define N 1024

#define TEST(SIGN)                                             \
  SIGN char a_##SIGN[N], b_##SIGN[N];                          \
  int f_##SIGN (void)                                          \
  {                                                            \
    int i, sum = 0;                                            \
    for (i = 0; i < N; i++)                                    \
      sum += __builtin_abs (a_##SIGN[i] - b_##SIGN[i]);;       \
    return sum;                                                \
  }

TEST(signed);
TEST(unsigned);

/* { dg-final { scan-assembler {\txvabsd.bu\t} } } */
/* { dg-final { scan-assembler {\txvabsd.b\t} } } */
