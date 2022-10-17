/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mno-ssse3" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#include "pr105354-1.c"
void
sse2_test (void)
{
  union128i_b a, b, res_ab, exp_ab;
  union128i_w c, d, res_cd, exp_cd;

  for (int i = 0; i != 16;i++)
    {
      a.a[i] = i;
      b.a[i] = i + 16;
      res_ab.a[i] = 0;
      exp_ab.a[i] = -1;
      if (i <= 8)
	{
	  c.a[i] = i;
	  d.a[i] = i + 8;
	  res_cd.a[i] = 0;
	  exp_cd.a[i] = -1;
	}
    }

  res_ab.x = (__m128i)foo ((v16qi)a.x, (v16qi)b.x);
  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
  if (check_union128i_b (exp_ab, res_ab.a))
    abort ();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22 };
  res_ab.x = (__m128i)foo1 ((v16qi)a.x, (v16qi)b.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21 };
  res_ab.x = (__m128i)foo2 ((v16qi)a.x, (v16qi)b.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22 };
  res_ab.x = (__m128i)foo3 ((v16qi)a.x, (v16qi)b.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  res_ab.x = (__m128i)foo8 ((v16qi)a.x);
  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4 };
  if (check_union128i_b (exp_ab, res_ab.a))
    abort ();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 2, 3, 4, 5, 6 };
  res_ab.x = (__m128i)foo9 ((v16qi)a.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 1, 2, 3, 4, 5 };
  res_ab.x = (__m128i)foo10 ((v16qi)a.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  exp_ab.x = __extension__(__m128i) (v16qi) { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1, 2, 3, 4, 5, 6 };
  res_ab.x = (__m128i)foo11 ((v16qi)a.x);
  if (check_union128i_b (exp_ab, res_ab.a))
    abort();

  res_cd.x = (__m128i)foo4 ((v8hi)c.x, (v8hi)d.x);
  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 7, 8, 9, 10, 11, 12 };
  if (check_union128i_w (exp_cd, res_cd.a))
    abort ();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 7, 9, 10, 11, 12, 13 };
  res_cd.x = (__m128i)foo5 ((v8hi)c.x, (v8hi)d.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 8, 9, 10, 11, 12, 13 };
  res_cd.x = (__m128i)foo6 ((v8hi)c.x, (v8hi)d.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

  res_cd.x = (__m128i)foo7 ((v8hi)c.x, (v8hi)d.x);
  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 9, 10, 11, 12, 13, 14 };
  if (check_union128i_w (exp_cd, res_cd.a))
    abort ();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 7, 0, 1, 2, 3, 4 };
  res_cd.x = (__m128i)foo12 ((v8hi)c.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 7, 1, 2, 3, 4, 5 };
  res_cd.x = (__m128i)foo13 ((v8hi)c.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 0, 1, 2, 3, 4, 5 };
  res_cd.x = (__m128i)foo14 ((v8hi)c.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

  exp_cd.x = __extension__(__m128i) (v8hi) { 5, 6, 1, 2, 3, 4, 5, 6 };
  res_cd.x = (__m128i)foo15 ((v8hi)c.x);
  if (check_union128i_w (exp_cd, res_cd.a))
    abort();

}

