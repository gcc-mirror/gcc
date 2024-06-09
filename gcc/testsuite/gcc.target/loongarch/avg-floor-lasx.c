/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "xvavg.b" } } */
/* { dg-final { scan-assembler "xvavg.bu" } } */
/* { dg-final { scan-assembler "xvavg.hu" } } */
/* { dg-final { scan-assembler "xvavg.h" } } */

#define N 1024

#define TEST(TYPE, NAME)                                        \
  TYPE a_##NAME[N], b_##NAME[N], c_##NAME[N];                   \
  void f_##NAME (void)                                          \
  {                                                             \
    int i;                                                      \
    for (i = 0; i < N; i++)                                     \
      a_##NAME[i] = (b_##NAME[i] + c_##NAME[i]) >> 1;           \
  }

TEST(char, 1);
TEST(short, 2);
TEST(unsigned char, 3);
TEST(unsigned short, 4);
