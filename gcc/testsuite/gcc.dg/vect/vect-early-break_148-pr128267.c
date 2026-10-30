/* PR tree-optimization/128267 */
/* { dg-do run } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target i?86-*-* x86_64-*-* } } */

/* Like vect-early-break_147-pr128267.c but with 16-bit IVs where in
   addition the number of iterations does not fit in the IV type and
   has to be handled before converting it to that type.  */

#include "tree-vect.h"

#define N 0x10040

unsigned short a[N];

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

TEST (lshr, unsigned short, >>=)
TEST (ashr, short, >>=)
TEST (shl, unsigned short, <<=)

static void
check (int k)
{
  a[k] = 42;
  if (lshr (0xffff) != lshr_ref (0xffff, k))
    __builtin_abort ();
  if (ashr (-0x8000) != ashr_ref (-0x8000, k))
    __builtin_abort ();
  if (ashr (0x7fff) != ashr_ref (0x7fff, k))
    __builtin_abort ();
  if (shl (0xffff) != shl_ref (0xffff, k))
    __builtin_abort ();
  a[k] = 0;
}

int main ()
{
  check_vect ();

#pragma GCC novector
  for (int k = 0; k < 0x40; k++)
    check (k);
#pragma GCC novector
  for (int k = 0x10000 - 0x10; k < N; k++)
    check (k);
  return 0;
}
