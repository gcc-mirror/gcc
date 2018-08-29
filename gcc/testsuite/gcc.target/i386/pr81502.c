/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

#define SIZE (sizeof (void *))

static int foo(unsigned char (*foo)[SIZE])
{
  __m128i acc = _mm_set_epi32(0, 0, 0, 0);
  size_t i = 0;
  for(; i + sizeof(__m128i) <= SIZE; i += sizeof(__m128i)) {
      __m128i word;
      __builtin_memcpy(&word, foo + i, sizeof(__m128i));
      acc = _mm_add_epi32(word, acc);
  }
  if (i != SIZE) {
      __m128i word = _mm_set_epi32(0, 0, 0, 0);
      __builtin_memcpy(&word, foo + i, SIZE - i); // (1)
      acc = _mm_add_epi32(word, acc);
  }
  int res;
  __builtin_memcpy(&res, &acc, sizeof(res));
  return res;
}

int bar(void *ptr)
{
  unsigned char buf[SIZE];
  __builtin_memcpy(buf, &ptr, SIZE);
  return foo((unsigned char(*)[SIZE])buf);
}

/* { dg-final { scan-assembler-times "mov" 1 } } */
