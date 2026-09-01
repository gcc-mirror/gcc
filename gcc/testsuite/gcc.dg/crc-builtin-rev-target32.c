/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-fdump-rtl-expand-details" } */

#include <stdint.h>

/* Due to constant folding, use a runtime arg to confirm crc table emitting */
int8_t rev_crc8_data8 (int8_t x)
{
  return __builtin_rev_crc8_data8 (x, 'a', 0x12);
}

int16_t rev_crc16_data8 (int16_t x)
{
  return __builtin_rev_crc16_data8 (x, 'a', 0x1021);
}

int16_t rev_crc16_data16 (int16_t x)
{
  return __builtin_rev_crc16_data16 (x, 0x3214, 0x1021);
}

int32_t rev_crc32_data8 (int32_t x)
{
  return __builtin_rev_crc32_data8 (x, 0x32, 0x4002123);
}

int32_t rev_crc32_data16 (int32_t x)
{
  return __builtin_rev_crc32_data16 (x, 0x3232, 0x4002123);
}

int32_t rev_crc32_data32 (int32_t x)
{
  return __builtin_rev_crc32_data32 (x, 0x123546ff, 0x4002123);
}

/* { dg-final { scan-rtl-dump ";; (?:using optab for|emitting reversed crc table) crc_8_polynomial_0x12" "expand" } } */
/* { dg-final { scan-rtl-dump ";; (?:using optab for|emitting reversed crc table) crc_16_polynomial_0x1021" "expand" } } */
/* { dg-final { scan-rtl-dump ";; (?:using optab for|emitting reversed crc table) crc_32_polynomial_0x4002123" "expand" } } */
