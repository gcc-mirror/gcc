/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "slp-19.c"

void __attribute__ ((optimize (0)))
f_golden (float *restrict a, float *restrict b, float *restrict c,
	  float *restrict d, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i * 4] = c[i * 4] + d[i * 4];
      a[i * 4 + 1] = c[i * 4] + d[i * 4 + 1];
      a[i * 4 + 2] = c[i * 4 + 2] + d[i * 4 + 2];
      a[i * 4 + 3] = c[i * 4 + 3] + d[i * 4 + 3];
      b[i * 4] = c[i * 4 + 2] + d[i * 4];
      b[i * 4 + 1] = c[i * 4 + 1] + d[i * 4 + 1];
      b[i * 4 + 2] = c[i * 4 + 3] + d[i * 4 + 2];
      b[i * 4 + 3] = c[i * 4 + 3] + d[i * 4 + 3];
    }
}

int
main (void)
{
#define RUN(NUM)                                                               \
  float a_##NUM[NUM * 4 + 4] = {0};                                            \
  float a_golden_##NUM[NUM * 4 + 4] = {0};                                     \
  float b_##NUM[NUM * 4 + 4] = {0};                                            \
  float b_golden_##NUM[NUM * 4 + 4] = {0};                                     \
  float c_##NUM[NUM * 4 + 4] = {0};                                            \
  float d_##NUM[NUM * 4 + 4] = {0};                                            \
  for (int i = 0; i < NUM * 4 + 4; i++)                                        \
    {                                                                          \
      c_##NUM[i] = i * 3.789 - 987.135;                                        \
      d_##NUM[i] = i * -13.789 + 1987.135;                                     \
    }                                                                          \
  f (a_##NUM, b_##NUM, c_##NUM, d_##NUM, NUM);                                 \
  f_golden (a_golden_##NUM, b_golden_##NUM, c_##NUM, d_##NUM, NUM);            \
  for (int i = 0; i < NUM * 4 + 4; i++)                                        \
    {                                                                          \
      if (a_##NUM[i] != a_golden_##NUM[i])                                     \
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
