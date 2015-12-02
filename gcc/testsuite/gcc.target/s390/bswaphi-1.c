/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -mzarch" } */

#include <stdint.h>

uint16_t u16;

uint16_t
foo16a (uint16_t a)
{
  return __builtin_bswap16 (a);
}
/* { dg-final { scan-assembler-times "lrvr\t%r2,%r\[0-9\]*" 1 } } */

uint16_t
foo16b ()
{
  return __builtin_bswap16 (u16);
}
/* { dg-final { scan-assembler-times "lrvh\t%r2,0\\(%r\[0-9\]*\\)" 1 } } */

void
foo16c (uint16_t a)
{
  u16 = __builtin_bswap16 (a);
}
/* { dg-final { scan-assembler-times "strvh\t%r2,0\\(%r\[0-9\]*\\)" 1 } } */
