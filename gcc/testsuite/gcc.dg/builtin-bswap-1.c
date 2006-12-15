/* { dg-do compile } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "" } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

#include <stdint.h>

uint32_t foo (uint32_t a)
{
  int b;

  b = __builtin_bswap32 (a);

  return b;
}
