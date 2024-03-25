/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=gnu99 -O2 -ftree-loop-distribution -fdump-tree-ldist-details" } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrQI" 2 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrHI" 2 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated rawmemchrSI" 2 "ldist" } } */

#include <string.h>
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#define rawmemchrT(T, pattern)     \
__attribute__((noinline,noclone))  \
T* rawmemchr_##T (T *s)            \
{                                  \
  while (*s != pattern)            \
    ++s;                           \
  return s;                        \
}

rawmemchrT(int8_t, (int8_t)0xde)
rawmemchrT(uint8_t, 0xde)
rawmemchrT(int16_t, (int16_t)0xdead)
rawmemchrT(uint16_t, 0xdead)
rawmemchrT(int32_t, (int32_t)0xdeadbeef)
rawmemchrT(uint32_t, 0xdeadbeef)

#define runT(T, pattern)                           \
void run_##T ()                                    \
{                                                  \
  T *buf = malloc (4096 * 2 * sizeof(T));          \
  assert (buf != NULL);                            \
  memset (buf, 0xa, 4096 * 2 * sizeof(T));         \
  /* ensure q is 4096-byte aligned */              \
  T *q = (T*)((unsigned char *)buf                 \
              + (4096 - ((uintptr_t)buf & 4095))); \
  T *p;                                            \
  /* unaligned + block boundary + 1st load */      \
  p = (T *) ((uintptr_t)q - 8);                    \
  p[2] = pattern;                                  \
  assert ((rawmemchr_##T (&p[0]) == &p[2]));       \
  p[2] = (T) 0xaaaaaaaa;                           \
  /* unaligned + block boundary + 2nd load */      \
  p = (T *) ((uintptr_t)q - 8);                    \
  p[6] = pattern;                                  \
  assert ((rawmemchr_##T (&p[0]) == &p[6]));       \
  p[6] = (T) 0xaaaaaaaa;                           \
  /* unaligned + 1st load */                       \
  q[5] = pattern;                                  \
  assert ((rawmemchr_##T (&q[2]) == &q[5]));       \
  q[5] = (T) 0xaaaaaaaa;                           \
  /* unaligned + 2nd load */                       \
  q[14] = pattern;                                 \
  assert ((rawmemchr_##T (&q[2]) == &q[14]));      \
  q[14] = (T) 0xaaaaaaaa;                          \
  /* unaligned + 3rd load */                       \
  q[19] = pattern;                                 \
  assert ((rawmemchr_##T (&q[2]) == &q[19]));      \
  q[19] = (T) 0xaaaaaaaa;                          \
  /* unaligned + 4th load */                       \
  q[25] = pattern;                                 \
  assert ((rawmemchr_##T (&q[2]) == &q[25]));      \
  q[25] = (T) 0xaaaaaaaa;                          \
  /* aligned + 1st load */                         \
  q[5] = pattern;                                  \
  assert ((rawmemchr_##T (&q[0]) == &q[5]));       \
  q[5] = (T) 0xaaaaaaaa;                           \
  /* aligned + 2nd load */                         \
  q[14] = pattern;                                 \
  assert ((rawmemchr_##T (&q[0]) == &q[14]));      \
  q[14] = (T) 0xaaaaaaaa;                          \
  /* aligned + 3rd load */                         \
  q[19] = pattern;                                 \
  assert ((rawmemchr_##T (&q[0]) == &q[19]));      \
  q[19] = (T) 0xaaaaaaaa;                          \
  /* aligned + 4th load */                         \
  q[25] = pattern;                                 \
  assert ((rawmemchr_##T (&q[0]) == &q[25]));      \
  q[25] = (T) 0xaaaaaaaa;                          \
  free (buf);                                      \
}

runT(int8_t, (int8_t)0xde)
runT(uint8_t, 0xde)
runT(int16_t, (int16_t)0xdead)
runT(uint16_t, 0xdead)
runT(int32_t, (int32_t)0xdeadbeef)
runT(uint32_t, 0xdeadbeef)

int main (void)
{
  run_uint8_t ();
  run_int8_t ();
  run_uint16_t ();
  run_int16_t ();
  run_uint32_t ();
  run_int32_t ();
  return 0;
}
