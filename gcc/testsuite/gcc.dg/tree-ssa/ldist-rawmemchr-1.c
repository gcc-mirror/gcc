/* { dg-do run { target { { s390x-*-* } || { riscv_v } } } } */
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */
/* { dg-additional-options "-march=z13 -mzarch" { target s390x-*-* } } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrQI" 2 "ldist" { target { { s390x-*-* } || { riscv_v } } } } } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrHI" 2 "ldist" { target { { s390x-*-* } || { riscv_v } } } } } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrSI" 2 "ldist" { target { { s390x-*-* } || { riscv_v } } } } } */

/* Rawmemchr pattern: reduction stmt and no store */

#include <stdint.h>
#include <assert.h>

typedef __SIZE_TYPE__ size_t;
extern void* malloc (size_t);
extern void* memset (void*, int, size_t);

#define test(T, pattern)   \
__attribute__((noinline))  \
T *test_##T (T *p)         \
{                          \
  while (*p != (T)pattern) \
    ++p;                   \
  return p;                \
}

test (uint8_t,  0xab)
test (uint16_t, 0xabcd)
test (uint32_t, 0xabcdef15)

test (int8_t,  0xab)
test (int16_t, 0xabcd)
test (int32_t, 0xabcdef15)

#define run(T, pattern, i)      \
{                               \
T *q = p;                       \
q[i] = (T)pattern;              \
assert (test_##T (p) == &q[i]); \
q[i] = 0;                       \
}

int main(void)
{
  void *p = malloc (1024);
  assert (p);
  memset (p, 0, 1024);

  run (uint8_t, 0xab, 0);
  run (uint8_t, 0xab, 1);
  run (uint8_t, 0xab, 13);

  run (uint16_t, 0xabcd, 0);
  run (uint16_t, 0xabcd, 1);
  run (uint16_t, 0xabcd, 13);

  run (uint32_t, 0xabcdef15, 0);
  run (uint32_t, 0xabcdef15, 1);
  run (uint32_t, 0xabcdef15, 13);

  run (int8_t, 0xab, 0);
  run (int8_t, 0xab, 1);
  run (int8_t, 0xab, 13);

  run (int16_t, 0xabcd, 0);
  run (int16_t, 0xabcd, 1);
  run (int16_t, 0xabcd, 13);

  run (int32_t, 0xabcdef15, 0);
  run (int32_t, 0xabcdef15, 1);
  run (int32_t, 0xabcdef15, 13);

  return 0;
}
