/* { dg-options "-mabicalls -mshared -mabi=n32" } */
/* { dg-final { scan-assembler-not "(\\\$25|\\\$28|%gp_rel|%got)" } } */
/* { dg-final { scan-assembler-not "\tjr\t\\\$1\n" } } */

#include "branch-helper.h"

NOMIPS16 void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fff8;
}
