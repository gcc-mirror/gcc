/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

#include <stdint.h>

uint8_t neg_u8 (const uint8_t src)
{
  return ~src;
}

/* { dg-final { scan-assembler-times "xori\t" 1 } } */
/* { dg-final { scan-assembler-not "not\t" } } */
/* { dg-final { scan-assembler-not "andi\t" } } */
