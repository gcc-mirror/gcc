/* PR target/120719 */
/* { dg-do compile } */
/* { dg-options "-O2 -mcrc32" } */

#include <stdint-gcc.h>

int32_t rev_crc32_data8 (int8_t v)
{
  return __builtin_rev_crc32_data8 (0xffffffff, v, 0x1EDC6F41);
}

int32_t rev_crc32_data16 (int16_t v)
{
  return __builtin_rev_crc32_data16 (0xffffffff, v, 0x1EDC6F41);
}

int32_t rev_crc32_data32 (int32_t v)
{
  return __builtin_rev_crc32_data32 (0xffffffff, v, 0x1EDC6F41);
} 

/* { dg-final { scan-assembler-times "\tcrc32" 3 } } */
