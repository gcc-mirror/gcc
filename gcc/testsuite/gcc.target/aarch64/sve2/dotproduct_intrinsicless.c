/* { dg-do compile } */
/* { dg-options "-O3 -march=armv9-a+sve2p3 -mautovec-preference=sve-only --param=aarch64-vect-unroll-limit=1" } */

#include <stdint.h>
#include <stddef.h>
int16_t dot_s8_s8_to_s16(const int8_t *restrict a,
                          const int8_t *restrict b,
                          unsigned n) {
  int16_t acc = 0;


  for (unsigned i = 0; i < n; ++i)
    acc += (int16_t)a[i] * (int16_t)b[i];

  return acc;
}

uint16_t dot_u8_u8_to_u16(const uint8_t *restrict a,
                          const uint8_t *restrict b,
                          unsigned n) {
  uint16_t acc = 0;

  for (unsigned i = 0; i < n; ++i)
    acc += (uint16_t)a[i] * (uint16_t)b[i];

  return acc;
}

/* { dg-final { scan-assembler-times {\tsdot\tz[0-9]+.h, z[0-9]+\.b, z[0-9]+\.b} 1 } } */
/* { dg-final { scan-assembler-times {\tudot\tz[0-9]+.h, z[0-9]+\.b, z[0-9]+\.b} 1 } } */