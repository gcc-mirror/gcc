/* PR target/54700 */
/* { dg-do run { target sse4 } } */
/* { dg-options "-O2 -std=c++14 -msse4 -mno-avx -mno-xop" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include "sse4_1-pr54700-1.C"

static void
TEST ()
{
  __v16qi v16qia = { -128, 12, -1, 127, 115, 0, -19, 125, -125, 12, 0, -37, 37, 15, 98, -105 };
  __v16qi v16qib = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  __v16qi v16qic = { 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32 };
  __v16qi v16qie = { 1, 18, 3, 20, 21, 22, 7, 24, 9, 26, 27, 12, 29, 30, 31, 16 };
  __v16qi v16qif = { 17, 2, 19, 4, 5, 6, 23, 8, 25, 10, 11, 28, 13, 14, 15, 32 };
  __v16qi v16qir = f1 (v16qia, v16qib, v16qic);
  if (__builtin_memcmp (&v16qir, &v16qie, sizeof (__v16qi)))
    __builtin_abort ();
  v16qir = f2 (v16qia, v16qib, v16qic);
  if (__builtin_memcmp (&v16qir, &v16qif, sizeof (__v16qi)))
    __builtin_abort ();
  __v4si v4sia = { __INT_MAX__, -__INT_MAX__ - 1, -32, 12 };
  __v4si v4sib = { 1, 2, 3, 4 };
  __v4si v4sic = { 5, 6, 7, 8 };
  __v4si v4sie = { 5, 2, 3, 8 };
  __v4si v4sif = { 1, 6, 7, 4 };
  __v4si v4sir = f3 (v4sia, v4sib, v4sic);
  if (__builtin_memcmp (&v4sir, &v4sie, sizeof (__v4si)))
    __builtin_abort ();
  v4sir = f4 (v4sia, v4sib, v4sic);
  if (__builtin_memcmp (&v4sir, &v4sif, sizeof (__v4si)))
    __builtin_abort ();
  __v2di v2dia = { -__LONG_LONG_MAX__, 1000LL * __INT_MAX__ };
  __v2di v2dib = { 1, 2 };
  __v2di v2dic = { 3, 4 };
  __v2di v2die = { 1, 4 };
  __v2di v2dif = { 3, 2 };
  __v2di v2dir = f5 (v2dia, v2dib, v2dic);
  if (__builtin_memcmp (&v2dir, &v2die, sizeof (__v2di)))
    __builtin_abort ();
  v2dir = f6 (v2dia, v2dib, v2dic);
  if (__builtin_memcmp (&v2dir, &v2dif, sizeof (__v2di)))
    __builtin_abort ();
  __v4sf v4sfb = { 1.0f, 2.0f, 3.0f, 4.0f };
  __v4sf v4sfc = { 5.0f, 6.0f, 7.0f, 8.0f };
  __v4sf v4sfe = { 5.0f, 2.0f, 3.0f, 8.0f };
  __v4sf v4sff = { 1.0f, 6.0f, 7.0f, 4.0f };
  __v4sf v4sfr = f7 (v4sia, v4sfb, v4sfc);
  if (__builtin_memcmp (&v4sfr, &v4sfe, sizeof (__v4sf)))
    __builtin_abort ();
  v4sfr = f8 (v4sia, v4sfb, v4sfc);
  if (__builtin_memcmp (&v4sfr, &v4sff, sizeof (__v4sf)))
    __builtin_abort ();
  __v2df v2dfb = { 1.0, 2.0 };
  __v2df v2dfc = { 3.0, 4.0 };
  __v2df v2dfe = { 1.0, 4.0 };
  __v2df v2dff = { 3.0, 2.0 };
  __v2df v2dfr = f9 (v2dia, v2dfb, v2dfc);
  if (__builtin_memcmp (&v2dfr, &v2dfe, sizeof (__v2df)))
    __builtin_abort ();
  v2dfr = f10 (v2dia, v2dfb, v2dfc);
  if (__builtin_memcmp (&v2dfr, &v2dff, sizeof (__v2df)))
    __builtin_abort ();
}
