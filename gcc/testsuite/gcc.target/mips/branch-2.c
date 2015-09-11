/* { dg-options "-mshared -mabi=32" } */
/* { dg-final { scan-assembler-not "(\\\$25|\\\$28|cpload)" } } */
/* { dg-final { scan-assembler-not "\tjr\t\\\$1\n" } } */
/* { dg-final { scan-assembler-not "\\.cprestore" } } */

#include "branch-helper.h"

NOCOMPRESSION void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fff8;
}
