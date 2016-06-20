/* PR target/71559 */
/* { dg-do run { target sse2 } } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#ifndef PR71559_TEST
#include "sse2-check.h"
#define PR71559_TEST sse2_test
#endif

#define N 16
float a[N] = { 5.0f, -3.0f, 1.0f, __builtin_nanf (""), 9.0f, 7.0f, -3.0f, -9.0f,
               -3.0f, -5.0f, -9.0f, __builtin_nanf (""), 0.5f, -0.5f, 0.0f, 0.0f };
float b[N] = { -5.0f, 3.0f, 1.0f, 7.0f, 8.0f, 8.0f, -3.0f, __builtin_nanf (""),
               -4.0f, -4.0f, -9.0f, __builtin_nanf (""), 0.0f, 0.0f, 0.0f, __builtin_nanf ("") };
int c[N], d[N];

#define FN(name, op) \
void					\
name (void)				\
{					\
  int i;				\
  for (i = 0; i < N; i++)		\
    c[i] = (op || d[i] > 37) ? 5 : 32;	\
}
FN (eq, a[i] == b[i])
FN (ne, a[i] != b[i])
FN (gt, a[i] > b[i])
FN (ge, a[i] >= b[i])
FN (lt, a[i] < b[i])
FN (le, a[i] <= b[i])
FN (unle, !__builtin_isgreater (a[i], b[i]))
FN (unlt, !__builtin_isgreaterequal (a[i], b[i]))
FN (unge, !__builtin_isless (a[i], b[i]))
FN (ungt, !__builtin_islessequal (a[i], b[i]))
FN (uneq, !__builtin_islessgreater (a[i], b[i]))
FN (ordered, !__builtin_isunordered (a[i], b[i]))
FN (unordered, __builtin_isunordered (a[i], b[i]))

#define TEST(name, GT, LT, EQ, UO) \
  name ();				\
  for (i = 0; i < N; i++)		\
    {					\
      int v;				\
      switch (i % 4)			\
	{				\
	case 0: v = GT ? 5 : 32; break;	\
	case 1: v = LT ? 5 : 32; break;	\
	case 2: v = EQ ? 5 : 32; break;	\
	case 3: v = UO ? 5 : 32; break;	\
	}				\
      if (c[i] != v)			\
	__builtin_abort ();		\
    }

void
PR71559_TEST (void)
{
  int i;
  asm volatile ("" : : "g" (a), "g" (b), "g" (c), "g" (d) : "memory");
  TEST (eq, 0, 0, 1, 0)
  TEST (ne, 1, 1, 0, 1)
  TEST (gt, 1, 0, 0, 0)
  TEST (ge, 1, 0, 1, 0)
  TEST (lt, 0, 1, 0, 0)
  TEST (le, 0, 1, 1, 0)
  TEST (unle, 0, 1, 1, 1)
  TEST (unlt, 0, 1, 0, 1)
  TEST (unge, 1, 0, 1, 1)
  TEST (ungt, 1, 0, 0, 1)
  TEST (uneq, 0, 0, 1, 1)
  TEST (ordered, 1, 1, 1, 0)
  TEST (unordered, 0, 0, 0, 1)
}
