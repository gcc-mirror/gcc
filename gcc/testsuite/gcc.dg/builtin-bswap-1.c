/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "" } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

#include <stdint.h>

uint16_t foo16 (uint16_t a)
{
  uint16_t b;

  b = __builtin_bswap16 (a);

  return b;
}

uint32_t foo32 (uint32_t a)
{
  uint32_t b;

  b = __builtin_bswap32 (a);

  return b;
}

uint64_t foo64 (uint64_t a)
{
  uint64_t b;

  b = __builtin_bswap64 (a);

  return b;
}
