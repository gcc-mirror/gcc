/* PR middle-end/91582 - missing heap overflow detection for strcpy
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "../../gcc.dg/range.h"

#define INT_MAX     __INT_MAX__
#define INT_MIN     (-INT_MAX - 1)

extern "C" char* strcpy (char*, const char*);

void sink (void*);

#define S36 "0123456789abcdefghijklmnopqrstuvwxyz"
#define S(N) (S36 + sizeof S36 - N - 1)

#define T(src, alloc) do {			\
    const char *s = src;			\
    char *d = (char*)alloc;			\
    strcpy (d, s);				\
    sink (d);					\
  } while (0)


void test_strcpy_new_char (size_t n)
{
  size_t r_0_1 = UR (0, 1);
  size_t r_1_2 = UR (1, 2);
  size_t r_2_3 = UR (2, 3);

  T (S (0), new char[r_0_1]);
  T (S (1), new char[r_0_1]);       // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), new char[r_1_2]);
  T (S (1), new char[r_1_2]);
  T (S (2), new char[r_1_2]);       // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), new char[r_2_3]);
  T (S (2), new char[r_2_3]);
  T (S (3), new char[r_2_3]);       // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), new char[r_2_3]);       // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_2_smax = UR (2, SIZE_MAX);
  T (S (0), new char[r_2_smax]);
  T (S (1), new char[r_2_smax]);
  T (S (2), new char[r_2_smax]);
  T (S (3), new char[r_2_smax * 2]);
  T (S (4), new char[r_2_smax * 2 + 1]);

  T (S (1), new char[n]);
  T (S (2), new char[n + 1]);
  T (S (9), new char[n * 2 + 1]);

  int r_imin_imax = SR (INT_MIN, INT_MAX);
  T (S (1), new char[r_imin_imax]);
  T (S (2), new char[r_imin_imax + 1]);
  T (S (9), new char[r_imin_imax * 2 + 1]);

  int r_0_imax = SR (0, INT_MAX);
  T (S (1), new char[r_0_imax]);
  T (S (2), new char[r_0_imax + 1]);
  T (S (9), new char[r_0_imax * 2 + 1]);

  int r_1_imax = SR (1, INT_MAX);
  T (S (1), new char[r_1_imax]);
  T (S (2), new char[r_1_imax + 1]);
  T (S (9), new char[r_1_imax * 2 + 1]);

  ptrdiff_t r_dmin_dmax = SR (DIFF_MIN, DIFF_MAX);
  T (S (1), new char[r_dmin_dmax]);
  T (S (2), new char[r_dmin_dmax + 1]);
  T (S (9), new char[r_dmin_dmax * 2 + 1]);
}


void test_strcpy_new_char_array (size_t n)
{
  size_t r_0_1 = UR (0, 1);

  T (S (0), new char[r_0_1][1]);
  T (S (1), new char[r_0_1][1]);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (1), new char[r_0_1][2]);
  T (S (2), new char[r_0_1][2]);    // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_1_2 = UR (1, 2);
  T (S (0), new char[r_1_2][0]);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (0), new char[r_1_2][1]);
  T (S (1), new char[r_1_2][1]);
  T (S (2), new char[r_1_2][1]);    // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), new char[r_1_2][0]);    // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (0), new char[r_1_2][1]);
  T (S (1), new char[r_1_2][2]);
  T (S (3), new char[r_1_2][2]);
  T (S (4), new char[r_1_2][2]);    // { dg-warning "\\\[-Wstringop-overflow" }
}


#ifdef __INT16_TYPE__

// Hack around PR 92829.
#define XUR(min, max) \
  (++idx, (vals[idx] < min || max < vals[idx] ? min : vals[idx]))

typedef __INT16_TYPE__ int16_t;

void test_strcpy_new_int16_t (size_t n, const size_t vals[])
{
  size_t idx = 0;

  size_t r_0_1 = XUR (0, 1);
  size_t r_1_2 = XUR (1, 2);
  size_t r_2_3 = XUR (2, 3);

  T (S (0), new int16_t[r_0_1]);
  T (S (1), new int16_t[r_0_1]);
  T (S (2), new int16_t[r_0_1]);      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), new int16_t[r_1_2]);
  T (S (1), new int16_t[r_1_2]);
  T (S (2), new int16_t[r_1_2]);
  T (S (3), new int16_t[r_1_2]);
  T (S (4), new int16_t[r_1_2]);      // { dg-warning "\\\[-Wstringop-overflow" }

  T (S (0), new int16_t[r_2_3]);
  T (S (1), new int16_t[r_2_3]);
  T (S (5), new int16_t[r_2_3]);
  T (S (6), new int16_t[r_2_3]);      // { dg-warning "\\\[-Wstringop-overflow" }
  T (S (9), new int16_t[r_2_3]);      // { dg-warning "\\\[-Wstringop-overflow" }

  size_t r_2_smax = XUR (2, SIZE_MAX);
  T (S (0), new int16_t[r_2_smax]);
  T (S (1), new int16_t[r_2_smax]);
  T (S (2), new int16_t[r_2_smax]);
  T (S (3), new int16_t[r_2_smax * 2]);
  T (S (4), new int16_t[r_2_smax * 2 + 1]);

  T (S (1), new int16_t[n]);
  T (S (2), new int16_t[n + 1]);
  T (S (9), new int16_t[n * 2 + 1]);

  int r_imin_imax = SR (INT_MIN, INT_MAX);
  T (S (1), new int16_t[r_imin_imax]);
  T (S (2), new int16_t[r_imin_imax + 1]);
  T (S (9), new int16_t[r_imin_imax * 2 + 1]);

  int r_0_imax = SR (0, INT_MAX);
  T (S (1), new int16_t[r_0_imax]);
  T (S (2), new int16_t[r_0_imax + 1]);
  T (S (9), new int16_t[r_0_imax * 2 + 1]);

  int r_1_imax = SR (1, INT_MAX);
  T (S (1), new int16_t[r_1_imax]);
  T (S (2), new int16_t[r_1_imax + 1]);
  T (S (9), new int16_t[r_1_imax * 2 + 1]);

  ptrdiff_t r_dmin_dmax = SR (DIFF_MIN, DIFF_MAX);
  T (S (1), new int16_t[r_dmin_dmax]);
  T (S (2), new int16_t[r_dmin_dmax + 1]);
  T (S (9), new int16_t[r_dmin_dmax * 2 + 1]);
}

#endif   // int16_t
