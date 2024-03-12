#include <stddef.h>
#include <stdint-gcc.h>

#define test_1(TYPE1, TYPE2)                                                   \
  void __attribute__ ((noinline, noclone))                                     \
  test_1_##TYPE1_##TYPE2 (TYPE1 *__restrict f, TYPE2 *__restrict d, TYPE1 x,   \
			  TYPE1 x2, TYPE2 y, int n)                            \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	f[i * 2 + 0] = x;                                                      \
	f[i * 2 + 1] = x2;                                                     \
	d[i] = y;                                                              \
      }                                                                        \
  }

#define run_1(TYPE1, TYPE2)                                                    \
  int n_1_##TYPE1_##TYPE2 = 1;                                                 \
  TYPE1 x_1_##TYPE1 = 117;                                                     \
  TYPE1 x2_1_##TYPE1 = 232;                                                    \
  TYPE2 y_1_##TYPE2 = 9762;                                                    \
  TYPE1 f_1_##TYPE1[2 * 2 + 1] = {0};                                          \
  TYPE2 d_1_##TYPE2[2] = {0};                                                  \
  test_1_##TYPE1_##TYPE2 (f_1_##TYPE1, d_1_##TYPE2, x_1_##TYPE1, x2_1_##TYPE1, \
			  y_1_##TYPE2, n_1_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_1_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_1_##TYPE1[i * 2 + 0] != x_1_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 2 + 1] != x2_1_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i] != y_1_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_1_##TYPE1_##TYPE2; i < n_1_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_1_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_2(TYPE1, TYPE2)                                                    \
  int n_2_##TYPE1_##TYPE2 = 17;                                                \
  TYPE1 x_2_##TYPE1 = 133;                                                     \
  TYPE1 x2_2_##TYPE1 = 94;                                                     \
  TYPE2 y_2_##TYPE2 = 8672;                                                    \
  TYPE1 f_2_##TYPE1[18 * 2 + 1] = {0};                                         \
  TYPE2 d_2_##TYPE2[18] = {0};                                                 \
  test_1_##TYPE1_##TYPE2 (f_2_##TYPE1, d_2_##TYPE2, x_2_##TYPE1, x2_2_##TYPE1, \
			  y_2_##TYPE2, n_2_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_2_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_2_##TYPE1[i * 2 + 0] != x_2_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 2 + 1] != x2_2_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i] != y_2_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_2_##TYPE1_##TYPE2; i < n_2_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_2_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_3(TYPE1, TYPE2)                                                    \
  int n_3_##TYPE1_##TYPE2 = 32;                                                \
  TYPE1 x_3_##TYPE1 = 233;                                                     \
  TYPE1 x2_3_##TYPE1 = 78;                                                     \
  TYPE2 y_3_##TYPE2 = 1234;                                                    \
  TYPE1 f_3_##TYPE1[33 * 2 + 1] = {0};                                         \
  TYPE2 d_3_##TYPE2[33] = {0};                                                 \
  test_1_##TYPE1_##TYPE2 (f_3_##TYPE1, d_3_##TYPE2, x_3_##TYPE1, x2_3_##TYPE1, \
			  y_3_##TYPE2, n_3_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_3_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_3_##TYPE1[i * 2 + 0] != x_3_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 2 + 1] != x2_3_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i] != y_3_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_3_##TYPE1_##TYPE2; i < n_3_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_3_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_4(TYPE1, TYPE2)                                                    \
  int n_4_##TYPE1_##TYPE2 = 128;                                               \
  TYPE1 x_4_##TYPE1 = 222;                                                     \
  TYPE1 x2_4_##TYPE1 = 59;                                                     \
  TYPE2 y_4_##TYPE2 = 4321;                                                    \
  TYPE1 f_4_##TYPE1[129 * 2 + 1] = {0};                                        \
  TYPE2 d_4_##TYPE2[129] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_4_##TYPE1, d_4_##TYPE2, x_4_##TYPE1, x2_4_##TYPE1, \
			  y_4_##TYPE2, n_4_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_4_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_4_##TYPE1[i * 2 + 0] != x_4_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 2 + 1] != x2_4_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i] != y_4_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_4_##TYPE1_##TYPE2; i < n_4_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_4_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_5(TYPE1, TYPE2)                                                    \
  int n_5_##TYPE1_##TYPE2 = 177;                                               \
  TYPE1 x_5_##TYPE1 = 111;                                                     \
  TYPE1 x2_5_##TYPE1 = 189;                                                    \
  TYPE2 y_5_##TYPE2 = 5555;                                                    \
  TYPE1 f_5_##TYPE1[178 * 2 + 1] = {0};                                        \
  TYPE2 d_5_##TYPE2[178] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_5_##TYPE1, d_5_##TYPE2, x_5_##TYPE1, x2_5_##TYPE1, \
			  y_5_##TYPE2, n_5_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_5_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_5_##TYPE1[i * 2 + 0] != x_5_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 2 + 1] != x2_5_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i] != y_5_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_5_##TYPE1_##TYPE2; i < n_5_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_5_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_6(TYPE1, TYPE2)                                                    \
  int n_6_##TYPE1_##TYPE2 = 255;                                               \
  TYPE1 x_6_##TYPE1 = 123;                                                     \
  TYPE1 x2_6_##TYPE1 = 132;                                                    \
  TYPE2 y_6_##TYPE2 = 6655;                                                    \
  TYPE1 f_6_##TYPE1[256 * 2 + 1] = {0};                                        \
  TYPE2 d_6_##TYPE2[256] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_6_##TYPE1, d_6_##TYPE2, x_6_##TYPE1, x2_6_##TYPE1, \
			  y_6_##TYPE2, n_6_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_6_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_6_##TYPE1[i * 2 + 0] != x_6_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 2 + 1] != x2_6_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i] != y_6_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_6_##TYPE1_##TYPE2; i < n_6_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_6_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_7(TYPE1, TYPE2)                                                    \
  int n_7_##TYPE1_##TYPE2 = 333;                                               \
  TYPE1 x_7_##TYPE1 = 39;                                                      \
  TYPE1 x2_7_##TYPE1 = 59;                                                     \
  TYPE2 y_7_##TYPE2 = 5968;                                                    \
  TYPE1 f_7_##TYPE1[334 * 2 + 1] = {0};                                        \
  TYPE2 d_7_##TYPE2[334] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_7_##TYPE1, d_7_##TYPE2, x_7_##TYPE1, x2_7_##TYPE1, \
			  y_7_##TYPE2, n_7_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_7_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_7_##TYPE1[i * 2 + 0] != x_7_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 2 + 1] != x2_7_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i] != y_7_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_7_##TYPE1_##TYPE2; i < n_7_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_7_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_8(TYPE1, TYPE2)                                                    \
  int n_8_##TYPE1_##TYPE2 = 512;                                               \
  TYPE1 x_8_##TYPE1 = 71;                                                      \
  TYPE1 x2_8_##TYPE1 = 255;                                                    \
  TYPE2 y_8_##TYPE2 = 3366;                                                    \
  TYPE1 f_8_##TYPE1[513 * 2 + 1] = {0};                                        \
  TYPE2 d_8_##TYPE2[513] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_8_##TYPE1, d_8_##TYPE2, x_8_##TYPE1, x2_8_##TYPE1, \
			  y_8_##TYPE2, n_8_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_8_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_8_##TYPE1[i * 2 + 0] != x_8_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 2 + 1] != x2_8_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i] != y_8_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_8_##TYPE1_##TYPE2; i < n_8_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_8_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_9(TYPE1, TYPE2)                                                    \
  int n_9_##TYPE1_##TYPE2 = 637;                                               \
  TYPE1 x_9_##TYPE1 = 157;                                                     \
  TYPE1 x2_9_##TYPE1 = 89;                                                     \
  TYPE2 y_9_##TYPE2 = 5511;                                                    \
  TYPE1 f_9_##TYPE1[638 * 2 + 1] = {0};                                        \
  TYPE2 d_9_##TYPE2[638] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_9_##TYPE1, d_9_##TYPE2, x_9_##TYPE1, x2_9_##TYPE1, \
			  y_9_##TYPE2, n_9_##TYPE1_##TYPE2);                   \
  for (int i = 0; i < n_9_##TYPE1_##TYPE2; ++i)                                \
    {                                                                          \
      if (f_9_##TYPE1[i * 2 + 0] != x_9_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 2 + 1] != x2_9_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i] != y_9_##TYPE2)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_9_##TYPE1_##TYPE2; i < n_9_##TYPE1_##TYPE2 + 1; ++i)          \
    {                                                                          \
      if (f_9_##TYPE1[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_10(TYPE1, TYPE2)                                                   \
  int n_10_##TYPE1_##TYPE2 = 777;                                              \
  TYPE1 x_10_##TYPE1 = 203;                                                    \
  TYPE1 x2_10_##TYPE1 = 200;                                                   \
  TYPE2 y_10_##TYPE2 = 2023;                                                   \
  TYPE1 f_10_##TYPE1[778 * 2 + 1] = {0};                                       \
  TYPE2 d_10_##TYPE2[778] = {0};                                               \
  test_1_##TYPE1_##TYPE2 (f_10_##TYPE1, d_10_##TYPE2, x_10_##TYPE1,            \
			  x2_10_##TYPE1, y_10_##TYPE2, n_10_##TYPE1_##TYPE2);  \
  for (int i = 0; i < n_10_##TYPE1_##TYPE2; ++i)                               \
    {                                                                          \
      if (f_10_##TYPE1[i * 2 + 0] != x_10_##TYPE1)                             \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 2 + 1] != x2_10_##TYPE1)                            \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i] != y_10_##TYPE2)                                     \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_10_##TYPE1_##TYPE2; i < n_10_##TYPE1_##TYPE2 + 1; ++i)        \
    {                                                                          \
      if (f_10_##TYPE1[i * 2 + 0] != 0)                                        \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 2 + 1] != 0)                                        \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i] != 0)                                                \
	__builtin_abort ();                                                    \
    }

#define TEST_ALL(T)                                                            \
  T (int8_t, int16_t)                                                          \
  T (uint8_t, uint16_t)                                                        \
  T (int16_t, int32_t)                                                         \
  T (uint16_t, uint32_t)                                                       \
  T (int32_t, int64_t)                                                         \
  T (uint32_t, uint64_t)                                                       \
  T (float, double)
