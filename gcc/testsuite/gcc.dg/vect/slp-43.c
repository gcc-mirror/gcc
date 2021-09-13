/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fno-version-loops-for-strides" } */

#include <string.h>
#include "tree-vect.h"

/* AVX512BW has V64QImode, make char_1 vectorizable with that.  */
#define MAX_VEC_ELEMENTS 64

#define FOO(T,N) \
void __attribute__((noinline,noclone)) \
foo_ ## T ## _ ## N (T * __restrict__ in_, T * __restrict__ out_, int s) \
{ \
  T *in = __builtin_assume_aligned (in_, __BIGGEST_ALIGNMENT__); \
  T *out = __builtin_assume_aligned (out_, __BIGGEST_ALIGNMENT__); \
  for (int i = 0; i < MAX_VEC_ELEMENTS; i++) \
    { \
      for (int j = 0; j < N; ++j) \
        out[j] = in[j]; \
      in += s*N; \
      out += N; \
    } \
}

#define TEST(T,N) \
 do { \
  memset (out, 0, 4096); \
  foo_ ## T ## _ ## N ((T *)in, (T *)out, 1); \
  if (memcmp (in, out, sizeof (T) * MAX_VEC_ELEMENTS * N) != 0) \
    __builtin_abort (); \
  for (int i = sizeof (T) * MAX_VEC_ELEMENTS * N; i < 4096; ++i) \
    if (out[i] != 0) \
      __builtin_abort (); \
 } while (0)

FOO(char, 1)
FOO(char, 2)
FOO(char, 3)
FOO(char, 4)
FOO(char, 6)
FOO(char, 8)
FOO(int, 1)
FOO(int, 2)
FOO(int, 3)
FOO(int, 4)
FOO(int, 6)
FOO(int, 8)
FOO(int, 16)

char in[4096] __attribute__((aligned(__BIGGEST_ALIGNMENT__)));
char out[4096] __attribute__((aligned(__BIGGEST_ALIGNMENT__)));

int main()
{
  check_vect ();

  for (int i = 0; i < 4096; ++i)
    {
      in[i] = i;
      __asm__ volatile ("" : : : "memory");
    }

  TEST(char, 1);
  TEST(char, 2);
  TEST(char, 3);
  TEST(char, 4);
  TEST(char, 6);
  TEST(char, 8);
  TEST(int, 1);
  TEST(int, 2);
  TEST(int, 3);
  TEST(int, 4);
  TEST(int, 6);
  TEST(int, 8);
  TEST(int, 16);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 13 "vect" { target vect_hw_misalign } } } */
/* For ! vect_hw_misalign it depends on vector size and actual alignment
   requirements of the target which functions can be vectorized.  Avoid
   that bean-counting and per-target listing here.  */
