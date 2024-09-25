/* PR target/54700 */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -std=c++14 -mavx2 -mno-xop -mno-avx512f" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#ifndef CHECK_H
#define CHECK_H "avx2-check.h"
#endif

#ifndef TEST
#define TEST avx2_test
#endif

#include CHECK_H

#include "avx2-pr54700-1.C"

static void
TEST ()
{
  __v32qi v32qia = { -128, 12, -1, 127, 115, 0, -19, 125, -125, 12, 0, -37, 37, 15, 98, -105,
		     0, 1, 2, 3, -1, -2, -3, -4, 4, -5, 5, -6, 6, -7, 7, -8 };
  __v32qi v32qib = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
		     -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16 };
  __v32qi v32qic = { 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
		     -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29, -30, -31, -32 };
  __v32qi v32qie = { 1, 18, 3, 20, 21, 22, 7, 24, 9, 26, 27, 12, 29, 30, 31, 16,
		     -17, -18, -19, -20, -5, -6, -7, -8, -25, -10, -27, -12, -29, -14, -31, -16 };
  __v32qi v32qif = { 17, 2, 19, 4, 5, 6, 23, 8, 25, 10, 11, 28, 13, 14, 15, 32,
		     -1, -2, -3, -4, -21, -22, -23, -24, -9, -26, -11, -28, -13, -30, -15, -32 };
  __v32qi v32qir = f1 (v32qia, v32qib, v32qic);
  if (__builtin_memcmp (&v32qir, &v32qie, sizeof (__v32qi)))
    __builtin_abort ();
  v32qir = f2 (v32qia, v32qib, v32qic);
  if (__builtin_memcmp (&v32qir, &v32qif, sizeof (__v32qi)))
    __builtin_abort ();
  __v8si v8sia = { __INT_MAX__, -__INT_MAX__ - 1, -32, 12, __INT_MAX__ - 2, -__INT_MAX__, 15, -1 };
  __v8si v8sib = { 1, 2, 3, 4, -1, -2, -3, -4 };
  __v8si v8sic = { 5, 6, 7, 8, -5, -6, -7, -8 };
  __v8si v8sie = { 5, 2, 3, 8, -5, -2, -7, -4 };
  __v8si v8sif = { 1, 6, 7, 4, -1, -6, -3, -8 };
  __v8si v8sir = f3 (v8sia, v8sib, v8sic);
  if (__builtin_memcmp (&v8sir, &v8sie, sizeof (__v8si)))
    __builtin_abort ();
  v8sir = f4 (v8sia, v8sib, v8sic);
  if (__builtin_memcmp (&v8sir, &v8sif, sizeof (__v8si)))
    __builtin_abort ();
  __v4di v4dia = { -__LONG_LONG_MAX__, 1000LL * __INT_MAX__, __LONG_LONG_MAX__, -2 };
  __v4di v4dib = { 1, 2, -1, -2 };
  __v4di v4dic = { 3, 4, -3, -4 };
  __v4di v4die = { 1, 4, -3, -2 };
  __v4di v4dif = { 3, 2, -1, -4 };
  __v4di v4dir = f5 (v4dia, v4dib, v4dic);
  if (__builtin_memcmp (&v4dir, &v4die, sizeof (__v4di)))
    __builtin_abort ();
  v4dir = f6 (v4dia, v4dib, v4dic);
  if (__builtin_memcmp (&v4dir, &v4dif, sizeof (__v4di)))
    __builtin_abort ();
  __v8sf v8sfb = { 1.0f, 2.0f, 3.0f, 4.0f, -1.0f, -2.0f, -3.0f, -4.0f };
  __v8sf v8sfc = { 5.0f, 6.0f, 7.0f, 8.0f, -5.0f, -6.0f, -7.0f, -8.0f };
  __v8sf v8sfe = { 5.0f, 2.0f, 3.0f, 8.0f, -5.0f, -2.0f, -7.0f, -4.0f };
  __v8sf v8sff = { 1.0f, 6.0f, 7.0f, 4.0f, -1.0f, -6.0f, -3.0f, -8.0f };
  __v8sf v8sfr = f7 (v8sia, v8sfb, v8sfc);
  if (__builtin_memcmp (&v8sfr, &v8sfe, sizeof (__v8sf)))
    __builtin_abort ();
  v8sfr = f8 (v8sia, v8sfb, v8sfc);
  if (__builtin_memcmp (&v8sfr, &v8sff, sizeof (__v8sf)))
    __builtin_abort ();
  __v4df v4dfb = { 1.0, 2.0, -1.0, -2.0 };
  __v4df v4dfc = { 3.0, 4.0, -3.0, -4.0 };
  __v4df v4dfe = { 1.0, 4.0, -3.0, -2.0 };
  __v4df v4dff = { 3.0, 2.0, -1.0, -4.0 };
  __v4df v4dfr = f9 (v4dia, v4dfb, v4dfc);
  if (__builtin_memcmp (&v4dfr, &v4dfe, sizeof (__v4df)))
    __builtin_abort ();
  v4dfr = f10 (v4dia, v4dfb, v4dfc);
  if (__builtin_memcmp (&v4dfr, &v4dff, sizeof (__v4df)))
    __builtin_abort ();
}
