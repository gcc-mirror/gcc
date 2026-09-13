/* { dg-do compile { target { rv32 && { ! riscv_abi_e } } } } */
/* { dg-options "-march=rv32gc_zbc" } */

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

int32_t rev_crc32_data8 (int32_t x)
{
  return __builtin_rev_crc32_data8 (x, 'a', 0x04C11DB7);
}

int32_t rev_crc32_data16 (int32_t x)
{
  return __builtin_rev_crc32_data16 (x, 0x3214, 0x04C11DB7);
}

int32_t rev_crc32_data32 (int32_t x)
{
  return __builtin_rev_crc32_data32 (x, 0x123546ff, 0x04C11DB7);
}

/* { dg-final { scan-assembler-times "clmul\t" 9 } } */
/* { dg-final { scan-assembler-times "clmulr\t" 3 } } */
