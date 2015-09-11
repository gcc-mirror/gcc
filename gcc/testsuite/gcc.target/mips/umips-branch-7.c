/* { dg-options "-mshared -mabi=n32 -mmicromips" } */
/* { dg-final { scan-assembler-not "(\\\$25|\\\$28|%gp_rel|%got)" } } */
/* { dg-final { scan-assembler-not "\tjrc?\t\\\$1\n" } } */

#include "branch-helper.h"

NOMIPS16 void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0xfffc;
}
