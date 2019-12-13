/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -ftree-vectorize -mdejagnu-cpu=power8 -fno-vect-cost-model -fdump-tree-vect-details" } */

/* To test condition reduction vectorization, where comparison operands are of
   unsigned long long type and condition true/false values are integer type.  */

#include <math.h>

extern void
abort (void) __attribute__ ((noreturn));

#define N 27
#define FP_TYPE unsigned long long

__attribute__ ((noinline)) int
test_eq (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] == min_v)
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_ne (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] != min_v)
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_gt (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] > min_v)
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_ge (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] >= min_v)
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_lt (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] < min_v)
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_le (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (a[i] <= min_v)
      last = i;

  return last;
}

int
main (void)
{
  int ret = 0;

  FP_TYPE a1[N] = {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1,  2,  3, 4,
		   5,  6,  7,  8,  9,  10, 21, 22, 23, 24, 25, 26, 27};

  FP_TYPE a2[N] = {21, 22, 23, 24, 25, 26, 27, 28, 29, 10, 11, 12, 13, 14,
		   15, 16, 17, 18, 19, 20, 1,  2,  3,  4,  5,  6,  7};

  ret = test_eq (a1, 10);
  if (ret != 19)
    abort ();

  ret = test_ne (a1, 10);
  if (ret != 26)
    abort ();

  ret = test_gt (a2, 10);
  if (ret != 19)
    abort ();

  ret = test_ge (a2, 10);
  if (ret != 19)
    abort ();

  ret = test_lt (a1, 10);
  if (ret != 18)
    abort ();

  ret = test_le (a1, 10);
  if (ret != 19)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 6 "vect" } } */
