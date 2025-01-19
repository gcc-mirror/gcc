/* { dg-do compile { target { rv32 && { ! riscv_abi_e } } } } */
/* { dg-options "-march=rv32gc_zbc" } */

#include <stdint-gcc.h>

int8_t crc8_data8 ()
{
  return __builtin_crc8_data8 (0x34, 'a', 0x12);
}

int16_t crc16_data8 ()
{
  return __builtin_crc16_data8 (0x1234, 'a', 0x1021);
}

int16_t crc16_data16 ()
{
  return __builtin_crc16_data16 (0x1234, 0x3214, 0x1021);
}

/* { dg-final { scan-assembler-times "clmul\t" 6 } } */
