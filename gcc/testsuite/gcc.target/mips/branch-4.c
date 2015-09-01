/* { dg-options "-mshared -mabi=n32" } */
/* { dg-final { scan-assembler-not "(\\\$25|\\\$28|%gp_rel|%got)" } } */
/* { dg-final { scan-assembler-not "\tjrc?\t\\\$1\n" } } */

#include "branch-helper.h"

NOCOMPRESSION void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fff8;
}
