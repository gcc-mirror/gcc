/* PR tree-optimization/128267 */
/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target i?86-*-* x86_64-*-* } } */

/* The value of a shift IV after an early exit has to be computed
   correctly when the accumulated shift amount reaches the precision
   of the IV.  This requires different handling for logical right
   shifts and left shifts (result 0) and arithmetic right shifts
   (result is the sign of the initial value).  */

#include "tree-vect.h"

#define N 1000

unsigned a[N];

#define TEST(NAME, T, OP)				\
T b_##NAME[N];						\
							\
__attribute__((noipa)) T				\
NAME (T x)						\
{							\
  T v = x;						\
  for (int i = 0; i < N; i++)				\
    {							\
      if (a[i] == 42)					\
	return v;					\
      b_##NAME[i] = v;					\
      v OP 1;						\
    }							\
  return v;						\
}							\
							\
__attribute__((noipa)) T				\
NAME##_ref (T x, int k)					\
{							\
  T v = x;						\
_Pragma ("GCC novector")				\
  for (int i = 0; i < k; i++)				\
    v OP 1;						\
  return v;						\
}

TEST (lshr, unsigned, >>=)
TEST (ashr, int, >>=)
TEST (shl, unsigned, <<=)

int main ()
{
  check_vect ();

#pragma GCC novector
  for (int k = 0; k < N; k++)
    {
      a[k] = 42;
      if (lshr (0xffffffffu) != lshr_ref (0xffffffffu, k))
	__builtin_abort ();
      if (ashr (-__INT_MAX__ - 1) != ashr_ref (-__INT_MAX__ - 1, k))
	__builtin_abort ();
      if (ashr (__INT_MAX__) != ashr_ref (__INT_MAX__, k))
	__builtin_abort ();
      if (shl (0xffffffffu) != shl_ref (0xffffffffu, k))
	__builtin_abort ();
      a[k] = 0;
    }
  return 0;
}
