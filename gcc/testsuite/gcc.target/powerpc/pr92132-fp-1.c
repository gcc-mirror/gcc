/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -ftree-vectorize -mvsx -fno-vect-cost-model -fdump-tree-vect-details" } */

/* To test condition reduction vectorization, where comparison operands are of
   double type and condition true/false values are integer type.  Cover all
   float point comparison codes.  */

#include <math.h>

extern void
abort (void) __attribute__ ((noreturn));

#define N 27
#define FP_TYPE double

#define LTGT(a, b) (__builtin_islessgreater ((a), (b)))
#define UNORD(a, b) (__builtin_isunordered ((a), (b)))
#define ORD(a, b) (!__builtin_isunordered ((a), (b)))
#define UNEQ(a, b) (!__builtin_islessgreater ((a), (b)))
#define UNGT(a, b) (!__builtin_islessequal ((a), (b)))
#define UNGE(a, b) (!__builtin_isless ((a), (b)))
#define UNLT(a, b) (!__builtin_isgreaterequal ((a), (b)))
#define UNLE(a, b) (!__builtin_isgreater ((a), (b)))

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

__attribute__ ((noinline)) int
test_ltgt (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (LTGT (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_ord (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (ORD (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_unord (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNORD (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_uneq (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNEQ (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_ungt (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNGT (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_unge (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNGE (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_unlt (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNLT (a[i], min_v))
      last = i;

  return last;
}

__attribute__ ((noinline)) int
test_unle (FP_TYPE *a, FP_TYPE min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
    if (UNLE (a[i], min_v))
      last = i;

  return last;
}

int
main (void)
{
  int ret = 0;

  FP_TYPE a1[N] = {11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1,  2,  3, 4,
		   5,  6,  7,  8,  9,  10, 21, 22, 23, 24, 25, 26, 27};

  FP_TYPE a2[N] = {11, 12, 13, 14, 15, 16, 17, 18, 19, 20,  1,  2,  3, 4,
		   5,  6,  7,  8,  9,  10, 21, 22, 23, NAN, 25, 26, 27};

  FP_TYPE a3[N] = {21, 22, 23, 24, 25, 26, 27, 28, 29, 10, 11, 12, 13, 14,
		   15, 16, 17, 18, 19, 20, 1,  2,  3,  4,  5,  6,  7};

  FP_TYPE a4[N] = {21, 22, 23, 24, 25, 26, 27, 28, 29, 10, 11,  12, 13, 14,
		   15, 16, 17, 18, 19, 20, 1,  2,  3,  4,  NAN, 6,  7};

  FP_TYPE a5[N] = {21, 22, 23, 24, 25, 26, 27, 28, 29, 10, 11,  12, 13, 14,
		   15, 16, 17, 18, 19, 20, 1,  2,  3,  4,  NAN, 10, 10};

  ret = test_eq (a1, 10);
  if (ret != 19)
    abort ();

  ret = test_ne (a1, 10);
  if (ret != 26)
    abort ();

  ret = test_gt (a3, 10);
  if (ret != 19)
    abort ();

  ret = test_ge (a3, 10);
  if (ret != 19)
    abort ();

  ret = test_lt (a1, 10);
  if (ret != 18)
    abort ();

  ret = test_le (a1, 10);
  if (ret != 19)
    abort ();

  ret = test_ltgt (a3, 10);
  if (ret != 26)
    abort ();

  ret = test_ltgt (a5, 10);
  if (ret != 23)
    abort ();

  ret = test_unord (a5, 10);
  if (ret != 24)
    abort ();

  ret = test_ord (a5, 10);
  if (ret != 26)
    abort ();

  ret = test_uneq (a1, 10);
  if (ret != 19)
    abort ();

  ret = test_uneq (a4, 10);
  if (ret != 24)
    abort ();

  ret = test_ungt (a3, 10);
  if (ret != 19)
    abort ();

  ret = test_ungt (a4, 10);
  if (ret != 24)
    abort ();

  ret = test_unge (a3, 10);
  if (ret != 19)
    abort ();

  ret = test_ungt (a4, 10);
  if (ret != 24)
    abort ();

  ret = test_unlt (a1, 10);
  if (ret != 18)
    abort ();

  ret = test_unlt (a2, 10);
  if (ret != 23)
    abort ();

  ret = test_unle (a1, 10);
  if (ret != 19)
    abort ();

  ret = test_unle (a2, 10);
  if (ret != 23)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 14 "vect" } } */
