/* { dg-do compile } */
/* { dg-options "-Os -mhw-div -mhw-mul -mhw-mulx" } */

#include <stdint.h>
#include <stddef.h>

void foo(const uint8_t* str, uint32_t* res)
{
  uint32_t rdVal0, rdVal1, rdVal2;
  rdVal0 = rdVal1 = rdVal2 = 0;
  unsigned c;
  for (;;) {
    c = *str++;
    unsigned dig = c - '0';
    if (dig > 9)
      break; // non-digit
    uint64_t x10;

    x10 = (uint64_t)rdVal0*10 + dig;
    rdVal0 = (uint32_t)x10;
    dig = (uint32_t)(x10 >> 32);

    x10 = (uint64_t)rdVal1*10 + dig;
    rdVal1 = (uint32_t)x10;
    dig = (uint32_t)(x10 >> 32);

    rdVal2 = rdVal2*10 + dig;
  }
  res[0] = rdVal0;
  res[1] = rdVal1;
  res[2] = rdVal2;
}

/* { dg-final { scan-assembler-times "mulxuu\t" 2 } } */
