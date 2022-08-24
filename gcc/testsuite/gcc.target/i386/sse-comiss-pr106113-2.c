/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#ifndef TEST
#define TEST sse_test
#endif

#include CHECK_H

#include <xmmintrin.h>

#define CMP(PRED, EXP) \
      res = _mm_comi##PRED##_ss (__A, __B);           \
        if (res != EXP)                               \
	    abort ();
static void 
__attribute__((noinline, unused))
do_check (float s1, float s2)
{
  __m128 __A = _mm_load_ss (&s1);
  __m128 __B = _mm_load_ss (&s2);
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
      float x1;
      float x2;
    }
  inputs[] =
    {
      { 4.3, 2.18 },
      { -4.3, 3.18 },
      { __builtin_nanf (""), -5.8 },
      { -4.8, __builtin_nansf ("") },
      { 3.8, __builtin_nansf ("") },
      { 4.2, 4.2 },
      { __builtin_nanf (""), __builtin_nansf ("") },
    };
  int i;

  for (i = 0; i < sizeof (inputs) / sizeof (inputs[0]); i++)
    do_check (inputs[i].x1, inputs[i].x2);
}
