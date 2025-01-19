/* { dg-options "-march=armv8-a+crypto" } */

#include <stdint-gcc.h>
int8_t crc8_data8 ()
{
  return __builtin_crc8_data8 ('a', 0xff, 0x12);
}
int16_t crc16_data8 ()
{
  return __builtin_crc16_data8 (0x1234, 'a', 0x1021);
}

int16_t crc16_data16 ()
{
  return __builtin_crc16_data16 (0x1234, 0x3214, 0x1021);
}

int32_t crc32_data8 ()
{
  return __builtin_crc32_data8 (0xffffffff, 0x32, 0x4002123);
}
int32_t crc32_data16 ()
{
  return __builtin_crc32_data16 (0xffffffff, 0x3232, 0x4002123);
}

int32_t crc32_data32 ()
{
  return __builtin_crc32_data32 (0xffffffff, 0x123546ff, 0x4002123);
}

int8_t rev_crc8_data8 ()
{
  return __builtin_rev_crc8_data8 (0x34, 'a', 0x12);
}

int16_t rev_crc16_data8 ()
{
  return __builtin_rev_crc16_data8 (0x1234, 'a', 0x1021);
}

int16_t rev_crc16_data16 ()
{
  return __builtin_rev_crc16_data16 (0x1234, 0x3214, 0x1021);
}

int32_t rev_crc32_data8 ()
{
  return __builtin_rev_crc32_data8 (0xffffffff, 0x32, 0x4002123);
}

int32_t rev_crc32_data16 ()
{
  return __builtin_rev_crc32_data16 (0xffffffff, 0x3232, 0x4002123);
}

int32_t rev_crc32_data32 ()
{
  return __builtin_rev_crc32_data32 (0xffffffff, 0x123546ff, 0x4002123);
} 
/* { dg-final { scan-assembler-times "pmull" 24 } } */