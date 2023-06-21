#include <stddef.h>
#include <stdint-gcc.h>

#define test_1(TYPE1, TYPE2, TYPE3)                                            \
  void __attribute__ ((noinline, noclone))                                     \
  test_1_##TYPE1_##TYPE2 (TYPE1 *__restrict f, TYPE2 *__restrict d,            \
			  TYPE3 *__restrict e, TYPE1 x, TYPE1 x2, TYPE1 x3,    \
			  TYPE1 x4, TYPE2 y, TYPE2 y2, TYPE3 z, int n)         \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	f[i * 4 + 0] = x;                                                      \
	f[i * 4 + 1] = x2;                                                     \
	f[i * 4 + 2] = x3;                                                     \
	f[i * 4 + 3] = x4;                                                     \
	d[i * 2 + 0] = y;                                                      \
	d[i * 2 + 1] = y2;                                                     \
	e[i] = z;                                                              \
      }                                                                        \
  }

#define run_1(TYPE1, TYPE2, TYPE3)                                             \
  int n_1_##TYPE1_##TYPE2_##TYPE3 = 1;                                         \
  TYPE1 x_1_##TYPE1 = 117;                                                     \
  TYPE1 x2_1_##TYPE1 = 232;                                                    \
  TYPE1 x3_1_##TYPE1 = 127;                                                    \
  TYPE1 x4_1_##TYPE1 = 11;                                                     \
  TYPE2 y_1_##TYPE2 = 9762;                                                    \
  TYPE2 y2_1_##TYPE2 = 6279;                                                   \
  TYPE3 z_1_##TYPE3 = 5891663;                                                 \
  TYPE1 f_1_##TYPE1[2 * 4 + 1] = {0};                                          \
  TYPE2 d_1_##TYPE2[2 * 2 + 1] = {0};                                          \
  TYPE3 e_1_##TYPE3[2] = {0};                                                  \
  test_1_##TYPE1_##TYPE2 (f_1_##TYPE1, d_1_##TYPE2, e_1_##TYPE3, x_1_##TYPE1,  \
			  x2_1_##TYPE1, x3_1_##TYPE1, x4_1_##TYPE1,            \
			  y_1_##TYPE2, y2_1_##TYPE2, z_1_##TYPE3,              \
			  n_1_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_1_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_1_##TYPE1[i * 4 + 0] != x_1_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 1] != x2_1_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 2] != x3_1_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 3] != x4_1_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i * 2 + 0] != y_1_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i * 2 + 1] != y2_1_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_1_##TYPE3[i] != z_1_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_1_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_1_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_1_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_1_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_1_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_1_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_2(TYPE1, TYPE2, TYPE3)                                             \
  int n_2_##TYPE1_##TYPE2_##TYPE3 = 17;                                        \
  TYPE1 x_2_##TYPE1 = 107;                                                     \
  TYPE1 x2_2_##TYPE1 = 202;                                                    \
  TYPE1 x3_2_##TYPE1 = 17;                                                     \
  TYPE1 x4_2_##TYPE1 = 53;                                                     \
  TYPE2 y_2_##TYPE2 = 5566;                                                    \
  TYPE2 y2_2_##TYPE2 = 7926;                                                   \
  TYPE3 z_2_##TYPE3 = 781545971;                                               \
  TYPE1 f_2_##TYPE1[18 * 4 + 1] = {0};                                         \
  TYPE2 d_2_##TYPE2[18 * 2 + 1] = {0};                                         \
  TYPE3 e_2_##TYPE3[18] = {0};                                                 \
  test_1_##TYPE1_##TYPE2 (f_2_##TYPE1, d_2_##TYPE2, e_2_##TYPE3, x_2_##TYPE1,  \
			  x2_2_##TYPE1, x3_2_##TYPE1, x4_2_##TYPE1,            \
			  y_2_##TYPE2, y2_2_##TYPE2, z_2_##TYPE3,              \
			  n_2_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_2_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_2_##TYPE1[i * 4 + 0] != x_2_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 1] != x2_2_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 2] != x3_2_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 3] != x4_2_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i * 2 + 0] != y_2_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i * 2 + 1] != y2_2_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_2_##TYPE3[i] != z_2_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_2_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_2_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_2_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_2_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_2_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_2_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_3(TYPE1, TYPE2, TYPE3)                                             \
  int n_3_##TYPE1_##TYPE2_##TYPE3 = 32;                                        \
  TYPE1 x_3_##TYPE1 = 109;                                                     \
  TYPE1 x2_3_##TYPE1 = 239;                                                    \
  TYPE1 x3_3_##TYPE1 = 151;                                                    \
  TYPE1 x4_3_##TYPE1 = 3;                                                      \
  TYPE2 y_3_##TYPE2 = 1234;                                                    \
  TYPE2 y2_3_##TYPE2 = 4321;                                                   \
  TYPE3 z_3_##TYPE3 = 145615615;                                               \
  TYPE1 f_3_##TYPE1[33 * 4 + 1] = {0};                                         \
  TYPE2 d_3_##TYPE2[33 * 2 + 1] = {0};                                         \
  TYPE3 e_3_##TYPE3[33] = {0};                                                 \
  test_1_##TYPE1_##TYPE2 (f_3_##TYPE1, d_3_##TYPE2, e_3_##TYPE3, x_3_##TYPE1,  \
			  x2_3_##TYPE1, x3_3_##TYPE1, x4_3_##TYPE1,            \
			  y_3_##TYPE2, y2_3_##TYPE2, z_3_##TYPE3,              \
			  n_3_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_3_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_3_##TYPE1[i * 4 + 0] != x_3_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 1] != x2_3_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 2] != x3_3_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 3] != x4_3_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i * 2 + 0] != y_3_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i * 2 + 1] != y2_3_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_3_##TYPE3[i] != z_3_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_3_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_3_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_3_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_3_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_3_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_3_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_4(TYPE1, TYPE2, TYPE3)                                             \
  int n_4_##TYPE1_##TYPE2_##TYPE3 = 128;                                       \
  TYPE1 x_4_##TYPE1 = 239;                                                     \
  TYPE1 x2_4_##TYPE1 = 132;                                                    \
  TYPE1 x3_4_##TYPE1 = 39;                                                     \
  TYPE1 x4_4_##TYPE1 = 48;                                                     \
  TYPE2 y_4_##TYPE2 = 1036;                                                    \
  TYPE2 y2_4_##TYPE2 = 3665;                                                   \
  TYPE3 z_4_##TYPE3 = 5145656;                                                 \
  TYPE1 f_4_##TYPE1[129 * 4 + 1] = {0};                                        \
  TYPE2 d_4_##TYPE2[129 * 2 + 1] = {0};                                        \
  TYPE3 e_4_##TYPE3[129] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_4_##TYPE1, d_4_##TYPE2, e_4_##TYPE3, x_4_##TYPE1,  \
			  x2_4_##TYPE1, x3_4_##TYPE1, x4_4_##TYPE1,            \
			  y_4_##TYPE2, y2_4_##TYPE2, z_4_##TYPE3,              \
			  n_4_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_4_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_4_##TYPE1[i * 4 + 0] != x_4_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 1] != x2_4_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 2] != x3_4_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 3] != x4_4_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i * 2 + 0] != y_4_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i * 2 + 1] != y2_4_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_4_##TYPE3[i] != z_4_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_4_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_4_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_4_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_4_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_4_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_4_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_5(TYPE1, TYPE2, TYPE3)                                             \
  int n_5_##TYPE1_##TYPE2_##TYPE3 = 177;                                       \
  TYPE1 x_5_##TYPE1 = 239;                                                     \
  TYPE1 x2_5_##TYPE1 = 132;                                                    \
  TYPE1 x3_5_##TYPE1 = 39;                                                     \
  TYPE1 x4_5_##TYPE1 = 48;                                                     \
  TYPE2 y_5_##TYPE2 = 1036;                                                    \
  TYPE2 y2_5_##TYPE2 = 3665;                                                   \
  TYPE3 z_5_##TYPE3 = 5145656;                                                 \
  TYPE1 f_5_##TYPE1[178 * 4 + 1] = {0};                                        \
  TYPE2 d_5_##TYPE2[178 * 2 + 1] = {0};                                        \
  TYPE3 e_5_##TYPE3[178] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_5_##TYPE1, d_5_##TYPE2, e_5_##TYPE3, x_5_##TYPE1,  \
			  x2_5_##TYPE1, x3_5_##TYPE1, x4_5_##TYPE1,            \
			  y_5_##TYPE2, y2_5_##TYPE2, z_5_##TYPE3,              \
			  n_5_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_5_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_5_##TYPE1[i * 4 + 0] != x_5_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 1] != x2_5_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 2] != x3_5_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 3] != x4_5_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i * 2 + 0] != y_5_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i * 2 + 1] != y2_5_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_5_##TYPE3[i] != z_5_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_5_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_5_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_5_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_5_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_5_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_5_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_6(TYPE1, TYPE2, TYPE3)                                             \
  int n_6_##TYPE1_##TYPE2_##TYPE3 = 255;                                       \
  TYPE1 x_6_##TYPE1 = 239;                                                     \
  TYPE1 x2_6_##TYPE1 = 132;                                                    \
  TYPE1 x3_6_##TYPE1 = 39;                                                     \
  TYPE1 x4_6_##TYPE1 = 48;                                                     \
  TYPE2 y_6_##TYPE2 = 1036;                                                    \
  TYPE2 y2_6_##TYPE2 = 3665;                                                   \
  TYPE3 z_6_##TYPE3 = 5145656;                                                 \
  TYPE1 f_6_##TYPE1[256 * 4 + 1] = {0};                                        \
  TYPE2 d_6_##TYPE2[256 * 2 + 1] = {0};                                        \
  TYPE3 e_6_##TYPE3[256] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_6_##TYPE1, d_6_##TYPE2, e_6_##TYPE3, x_6_##TYPE1,  \
			  x2_6_##TYPE1, x3_6_##TYPE1, x4_6_##TYPE1,            \
			  y_6_##TYPE2, y2_6_##TYPE2, z_6_##TYPE3,              \
			  n_6_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_6_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_6_##TYPE1[i * 4 + 0] != x_6_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 1] != x2_6_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 2] != x3_6_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 3] != x4_6_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i * 2 + 0] != y_6_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i * 2 + 1] != y2_6_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_6_##TYPE3[i] != z_6_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_6_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_6_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_6_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_6_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_6_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_6_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_7(TYPE1, TYPE2, TYPE3)                                             \
  int n_7_##TYPE1_##TYPE2_##TYPE3 = 333;                                       \
  TYPE1 x_7_##TYPE1 = 239;                                                     \
  TYPE1 x2_7_##TYPE1 = 132;                                                    \
  TYPE1 x3_7_##TYPE1 = 39;                                                     \
  TYPE1 x4_7_##TYPE1 = 48;                                                     \
  TYPE2 y_7_##TYPE2 = 1036;                                                    \
  TYPE2 y2_7_##TYPE2 = 3665;                                                   \
  TYPE3 z_7_##TYPE3 = 5145656;                                                 \
  TYPE1 f_7_##TYPE1[334 * 4 + 1] = {0};                                        \
  TYPE2 d_7_##TYPE2[334 * 2 + 1] = {0};                                        \
  TYPE3 e_7_##TYPE3[334] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_7_##TYPE1, d_7_##TYPE2, e_7_##TYPE3, x_7_##TYPE1,  \
			  x2_7_##TYPE1, x3_7_##TYPE1, x4_7_##TYPE1,            \
			  y_7_##TYPE2, y2_7_##TYPE2, z_7_##TYPE3,              \
			  n_7_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_7_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_7_##TYPE1[i * 4 + 0] != x_7_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 1] != x2_7_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 2] != x3_7_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 3] != x4_7_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i * 2 + 0] != y_7_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i * 2 + 1] != y2_7_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_7_##TYPE3[i] != z_7_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_7_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_7_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_7_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_7_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_7_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_7_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_8(TYPE1, TYPE2, TYPE3)                                             \
  int n_8_##TYPE1_##TYPE2_##TYPE3 = 512;                                       \
  TYPE1 x_8_##TYPE1 = 239;                                                     \
  TYPE1 x2_8_##TYPE1 = 132;                                                    \
  TYPE1 x3_8_##TYPE1 = 39;                                                     \
  TYPE1 x4_8_##TYPE1 = 48;                                                     \
  TYPE2 y_8_##TYPE2 = 1036;                                                    \
  TYPE2 y2_8_##TYPE2 = 3665;                                                   \
  TYPE3 z_8_##TYPE3 = 5145656;                                                 \
  TYPE1 f_8_##TYPE1[513 * 4 + 1] = {0};                                        \
  TYPE2 d_8_##TYPE2[513 * 2 + 1] = {0};                                        \
  TYPE3 e_8_##TYPE3[513] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_8_##TYPE1, d_8_##TYPE2, e_8_##TYPE3, x_8_##TYPE1,  \
			  x2_8_##TYPE1, x3_8_##TYPE1, x4_8_##TYPE1,            \
			  y_8_##TYPE2, y2_8_##TYPE2, z_8_##TYPE3,              \
			  n_8_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_8_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_8_##TYPE1[i * 4 + 0] != x_8_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 1] != x2_8_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 2] != x3_8_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 3] != x4_8_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i * 2 + 0] != y_8_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i * 2 + 1] != y2_8_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_8_##TYPE3[i] != z_8_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_8_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_8_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_8_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_8_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_8_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_8_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_9(TYPE1, TYPE2, TYPE3)                                             \
  int n_9_##TYPE1_##TYPE2_##TYPE3 = 637;                                       \
  TYPE1 x_9_##TYPE1 = 222;                                                     \
  TYPE1 x2_9_##TYPE1 = 111;                                                    \
  TYPE1 x3_9_##TYPE1 = 11;                                                     \
  TYPE1 x4_9_##TYPE1 = 7;                                                     \
  TYPE2 y_9_##TYPE2 = 2034;                                                    \
  TYPE2 y2_9_##TYPE2 = 6987;                                                   \
  TYPE3 z_9_##TYPE3 = 1564616;                                                 \
  TYPE1 f_9_##TYPE1[638 * 4 + 1] = {0};                                        \
  TYPE2 d_9_##TYPE2[638 * 2 + 1] = {0};                                        \
  TYPE3 e_9_##TYPE3[638] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_9_##TYPE1, d_9_##TYPE2, e_9_##TYPE3, x_9_##TYPE1,  \
			  x2_9_##TYPE1, x3_9_##TYPE1, x4_9_##TYPE1,            \
			  y_9_##TYPE2, y2_9_##TYPE2, z_9_##TYPE3,              \
			  n_9_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_9_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_9_##TYPE1[i * 4 + 0] != x_9_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 1] != x2_9_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 2] != x3_9_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 3] != x4_9_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i * 2 + 0] != y_9_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i * 2 + 1] != y2_9_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_9_##TYPE3[i] != z_9_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_9_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_9_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_9_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_9_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_9_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_9_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define run_10(TYPE1, TYPE2, TYPE3)                                             \
  int n_10_##TYPE1_##TYPE2_##TYPE3 = 777;                                       \
  TYPE1 x_10_##TYPE1 = 222;                                                     \
  TYPE1 x2_10_##TYPE1 = 111;                                                    \
  TYPE1 x3_10_##TYPE1 = 11;                                                     \
  TYPE1 x4_10_##TYPE1 = 7;                                                     \
  TYPE2 y_10_##TYPE2 = 2034;                                                    \
  TYPE2 y2_10_##TYPE2 = 6987;                                                   \
  TYPE3 z_10_##TYPE3 = 1564616;                                                 \
  TYPE1 f_10_##TYPE1[778 * 4 + 1] = {0};                                        \
  TYPE2 d_10_##TYPE2[778 * 2 + 1] = {0};                                        \
  TYPE3 e_10_##TYPE3[778] = {0};                                                \
  test_1_##TYPE1_##TYPE2 (f_10_##TYPE1, d_10_##TYPE2, e_10_##TYPE3, x_10_##TYPE1,  \
			  x2_10_##TYPE1, x3_10_##TYPE1, x4_10_##TYPE1,            \
			  y_10_##TYPE2, y2_10_##TYPE2, z_10_##TYPE3,              \
			  n_10_##TYPE1_##TYPE2_##TYPE3);                        \
  for (int i = 0; i < n_10_##TYPE1_##TYPE2_##TYPE3; ++i)                        \
    {                                                                          \
      if (f_10_##TYPE1[i * 4 + 0] != x_10_##TYPE1)                               \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 1] != x2_10_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 2] != x3_10_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 3] != x4_10_##TYPE1)                              \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i * 2 + 0] != y_10_##TYPE2)                               \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i * 2 + 1] != y2_10_##TYPE2)                              \
	__builtin_abort ();                                                    \
      if (e_10_##TYPE3[i] != z_10_##TYPE3)                                       \
	__builtin_abort ();                                                    \
    }                                                                          \
  for (int i = n_10_##TYPE1_##TYPE2_##TYPE3;                                    \
       i < n_10_##TYPE1_##TYPE2_##TYPE3 + 1; ++i)                               \
    {                                                                          \
      if (f_10_##TYPE1[i * 4 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 2] != 0)                                         \
	__builtin_abort ();                                                    \
      if (f_10_##TYPE1[i * 4 + 3] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i * 2 + 0] != 0)                                         \
	__builtin_abort ();                                                    \
      if (d_10_##TYPE2[i * 2 + 1] != 0)                                         \
	__builtin_abort ();                                                    \
      if (e_10_##TYPE3[i] != 0)                                                 \
	__builtin_abort ();                                                    \
    }

#define TEST_ALL(T)                                                            \
  T (int8_t, int16_t, int32_t)                                                 \
  T (uint8_t, uint16_t, uint32_t)                                              \
  T (int16_t, int32_t, int64_t)                                                \
  T (uint16_t, uint32_t, uint64_t)
