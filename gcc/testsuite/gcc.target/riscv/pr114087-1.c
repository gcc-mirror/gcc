/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d" } */

#include <stdbool.h>
#include <stdint.h>

static uint32_t mask1 = 0x55555FFF;
static uint32_t val1  = 0x14501DEF;

bool pred1a(uint32_t x) {
  return ((x & mask1) == val1);
}

/* { dg-final { scan-assembler  {or\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+} } } */
