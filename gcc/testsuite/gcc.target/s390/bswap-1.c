/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -mzarch" } */

#include <stdint.h>

uint64_t u64;
uint32_t u32;
uint16_t u16;

uint64_t
foo64a (uint64_t a)
{
  return __builtin_bswap64 (a);
}
/* { dg-final { scan-assembler-times "lrvgr\t%r2,%r2" 1 { target lp64 } } } */

uint64_t
foo64b ()
{
  return __builtin_bswap64 (u64);
}
/* { dg-final { scan-assembler-times "lrvg\t%r2,0\\(%r\[0-9\]*\\)" 1 { target lp64 } } } */

uint32_t
foo32 ()
{
  return __builtin_bswap32 (u32);
}
/* { dg-final { scan-assembler-times "lrv\t%r2,0\\(%r\[0-9\]*\\)" 1 } } */

uint16_t
foo16 ()
{
  return __builtin_bswap16 (u16);
}
/* { dg-final { scan-assembler-times "lrvh\t%r2,0\\(%r\[0-9\]*\\)" 1 } } */
