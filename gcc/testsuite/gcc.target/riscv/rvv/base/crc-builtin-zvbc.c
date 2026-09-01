/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvbc -mabi=lp64d" } */

#include <stdint-gcc.h>

/* Due to constant folding, use a runtime arg to confirm clmul emitting */
int8_t crc8_data8 (int8_t x)
{
  return __builtin_crc8_data8 (x, 'a', 0x12);
}

int16_t crc16_data8 (int16_t x)
{
  return __builtin_crc16_data8 (x, 'a', 0x1021);
}

int16_t crc16_data16 (int16_t x)
{
  return __builtin_crc16_data16 (x, 0x3214, 0x1021);
}

int32_t crc32_data8 (int32_t x)
{
  return __builtin_crc32_data8 (x, 0x32, 0x4002123);
}

int32_t crc32_data16 (int32_t x)
{
  return __builtin_crc32_data16 (x, 0x3232, 0x4002123);
}

int32_t crc32_data32 (int32_t x)
{
  return __builtin_crc32_data32 (x, 0x123546ff, 0x4002123);
}

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
/* { dg-final { scan-assembler-times "vclmul.vx" 12 } } */
/* { dg-final { scan-assembler-times "vclmulh.vx" 12 } } */
