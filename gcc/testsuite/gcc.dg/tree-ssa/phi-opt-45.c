/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1" } */

#include <stdint.h>

int8_t f1 (int8_t x)
{
  return x != 0 ? x - (int8_t)1 : 0;
}

int16_t f2 (int16_t x)
{
  return x != 0 ? x - (int16_t)1 : 0;
}

int32_t f3 (int32_t x)
{
  return x != 0 ? x - (int32_t)1 : 0;
}

int64_t f4 (int64_t x)
{
  return x != 0 ? x - (int64_t)1 : 0;
}

/* { dg-final { scan-tree-dump-not "goto" "phiopt1" } } */
