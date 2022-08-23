/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#ifndef TEST
#define TEST sse2_test
#endif

#include CHECK_H

#include <emmintrin.h>

#define CMP(PRED, EXP) \
      res = _mm_ucomi##PRED##_sd (__A, __B);           \
        if (res != EXP)                               \
	    abort ();
static void 
__attribute__((noinline, unused))
do_check (double s1, double s2)
{
  __m128d __A = _mm_load_sd (&s1);
  __m128d __B = _mm_load_sd (&s2);
  int res;
  
  CMP (eq, (!__builtin_isunordered (s1, s2) && s1 == s2));
  CMP (ge, (!__builtin_isunordered (s1, s2) && s1 >= s2));
  CMP (gt, (!__builtin_isunordered (s1, s2) && s1 > s2));
  CMP (lt, (!__builtin_isunordered (s1, s2) && s1 < s2));
  CMP (le, (!__builtin_isunordered (s1, s2) && s1 <= s2));
  CMP (neq, (__builtin_isunordered (s1, s2) || s1 != s2));
}

static void
TEST (void)
{
  struct
    {
      double x1;
      double x2;
    }
  inputs[] =
    {
      { 4.3, 2.18 },
      { -4.3, 3.18 },
      { __builtin_nan (""), -5.8 },
      { -4.8, __builtin_nans ("") },
      { 3.8, __builtin_nans ("") },
      { 4.2, 4.2 },
      { __builtin_nan (""), __builtin_nans ("") },
    };
  int i;

  for (i = 0; i < sizeof (inputs) / sizeof (inputs[0]); i++)
    do_check (inputs[i].x1, inputs[i].x2);
}
