/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

#include <stdint.h>

void bin2ascii(uint64_t val, char *dst) {
  const int64_t POW10_10 = ((int64_t)10) * 1000 * 1000 * 1000;
  int64_t hix = val / POW10_10;
  int64_t lox = val % POW10_10;
  int32_t v0 = hix / 100000;
  int32_t v1 = hix % 100000;
  int32_t v2 = lox / 100000;
  int32_t v3 = lox % 100000;
  for (int i = 4; i != 0; --i) {
    dst[i + 0 * 5] = v0 % 10 + '0';
    v0 /= 10;
    dst[i + 1 * 5] = v1 % 10 + '0';
    v1 /= 10;
    dst[i + 2 * 5] = v2 % 10 + '0';
    v2 /= 10;
    dst[i + 3 * 5] = v3 % 10 + '0';
    v3 /= 10;
  }
  dst[0 * 5] = v0 + '0';
  dst[1 * 5] = v1 + '0';
  dst[2 * 5] = v2 + '0';
  dst[3 * 5] = v3 + '0';
  dst[4 * 5] = 0;
}

/* { dg-final { scan-assembler-not "idiv" } } */
