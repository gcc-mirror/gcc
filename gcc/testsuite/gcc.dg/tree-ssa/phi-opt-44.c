/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1" } */

#include <stdint.h>

uint8_t f1 (uint8_t x)
{
  return x >= (uint8_t)1 ? x - (uint8_t)1 : 0;
}

uint16_t f2 (uint16_t x)
{
  return x >= (uint16_t)1 ? x - (uint16_t)1 : 0;
}

uint32_t f3 (uint32_t x)
{
  return x >= (uint32_t)1 ? x - (uint32_t)1 : 0;
}

uint64_t f4 (uint64_t x)
{
  return x >= (uint64_t)1 ? x - (uint64_t)1 : 0;
}

/* { dg-final { scan-tree-dump-not "goto" "phiopt1" } } */
