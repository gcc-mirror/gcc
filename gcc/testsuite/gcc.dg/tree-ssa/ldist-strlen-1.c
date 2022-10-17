/* { dg-do run } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */
/* { dg-additional-options "-march=z13 -mzarch" { target s390x-*-* } } */
/* { dg-final { scan-tree-dump-times "generated strlenQI\n" 4 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated strlenHI\n" 4 "ldist" { target s390x-*-* } } } */
/* { dg-final { scan-tree-dump-times "generated strlenSI\n" 4 "ldist" { target s390x-*-* } } } */

#include <stdint.h>
#include <assert.h>

typedef __SIZE_TYPE__ size_t;
extern void* malloc (size_t);
extern void* memset (void*, int, size_t);

#define test(T, U)        \
__attribute__((noinline)) \
U test_##T##U (T *s)      \
{                         \
  U i;                    \
  for (i=0; s[i]; ++i);   \
  return i;               \
}

test (uint8_t,  size_t)
test (uint16_t, size_t)
test (uint32_t, size_t)
test (uint8_t,  int)
test (uint16_t, int)
test (uint32_t, int)

test (int8_t,  size_t)
test (int16_t, size_t)
test (int32_t, size_t)
test (int8_t,  int)
test (int16_t, int)
test (int32_t, int)

#define run(T, U, i)             \
{                                \
T *q = p;                        \
q[i] = 0;                        \
assert (test_##T##U (p) == i);   \
memset (&q[i], 0xf, sizeof (T)); \
}

int main(void)
{
  void *p = malloc (1024);
  assert (p);
  memset (p, 0xf, 1024);

  run (uint8_t, size_t, 0);
  run (uint8_t, size_t, 1);
  run (uint8_t, size_t, 13);

  run (int8_t, size_t, 0);
  run (int8_t, size_t, 1);
  run (int8_t, size_t, 13);

  run (uint8_t, int, 0);
  run (uint8_t, int, 1);
  run (uint8_t, int, 13);

  run (int8_t, int, 0);
  run (int8_t, int, 1);
  run (int8_t, int, 13);

  run (uint16_t, size_t, 0);
  run (uint16_t, size_t, 1);
  run (uint16_t, size_t, 13);

  run (int16_t, size_t, 0);
  run (int16_t, size_t, 1);
  run (int16_t, size_t, 13);

  run (uint16_t, int, 0);
  run (uint16_t, int, 1);
  run (uint16_t, int, 13);

  run (int16_t, int, 0);
  run (int16_t, int, 1);
  run (int16_t, int, 13);

  run (uint32_t, size_t, 0);
  run (uint32_t, size_t, 1);
  run (uint32_t, size_t, 13);

  run (int32_t, size_t, 0);
  run (int32_t, size_t, 1);
  run (int32_t, size_t, 13);

  run (uint32_t, int, 0);
  run (uint32_t, int, 1);
  run (uint32_t, int, 13);

  run (int32_t, int, 0);
  run (int32_t, int, 1);
  run (int32_t, int, 13);

  return 0;
}
