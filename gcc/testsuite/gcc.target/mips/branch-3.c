/* { dg-options "-mabicalls -mshared -mabi=32" } */
/* { dg-final { scan-assembler "\t\\.cpload\t\\\$25\n" } } */
/* { dg-final { scan-assembler "\tjr\t\\\$1\n" } } */
/* { dg-final { scan-assembler-not "cprestore" } } */

#include "branch-helper.h"

NOMIPS16 void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fffc;
}
