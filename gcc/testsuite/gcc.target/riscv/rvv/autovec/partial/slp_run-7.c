/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "slp-7.c"

void
f_golden (float *__restrict f, double *__restrict d, int n)
{
  for (int i = 0; i < n; ++i)
    {
      f[i * 2 + 0] = 1;
      f[i * 2 + 1] = 2;
      d[i] = 3;
    }
}

int
main (void)
{
#define RUN(NUM)                                                               \
  float a_##NUM[NUM * 2 + 2] = {0};                                            \
  float a_golden_##NUM[NUM * 2 + 2] = {0};                                     \
  double b_##NUM[NUM] = {0};                                                   \
  double b_golden_##NUM[NUM] = {0};                                            \
  f (a_##NUM, b_##NUM, NUM);                                                   \
  f_golden (a_golden_##NUM, b_golden_##NUM, NUM);                              \
  for (int i = 0; i < NUM; i++)                                                \
    {                                                                          \
      if (a_##NUM[i * 2 + 0] != a_golden_##NUM[i * 2 + 0])                     \
	__builtin_abort ();                                                    \
      if (a_##NUM[i * 2 + 1] != a_golden_##NUM[i * 2 + 1])                     \
	__builtin_abort ();                                                    \
      if (b_##NUM[i] != b_golden_##NUM[i])                                     \
	__builtin_abort ();                                                    \
    }

  RUN (3);
  RUN (5);
  RUN (15);
  RUN (16);
  RUN (17);
  RUN (31);
  RUN (32);
  RUN (33);
  RUN (63);
  RUN (64);
  RUN (65);
  RUN (127);
  RUN (128);
  RUN (129);
  RUN (239);
  RUN (359);
  RUN (498);
  RUN (799);
  RUN (977);
  RUN (5789);
  return 0;
}
