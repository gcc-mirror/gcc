/* { dg-options "-O3 -mtune=neoverse-v1" } */

#include <stdint.h>

uint64_t f2(uint64_t *ptr, int n) {
  uint64_t res = 0;
  for (int i = 0; i < n; ++i)
    res += ptr[i];
  return res;
}

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 5 {target aarch64_big_endian} } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 1 {target aarch64_little_endian} } } */
/* { dg-final { scan-assembler-times {\tldr\tz[0-9]+,} 4 {target aarch64_little_endian} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d,} 8 } } */
