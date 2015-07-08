/* { dg-options "-mshared -mabi=32 -mmicromips" } */
/* { dg-final { scan-assembler "\t\\.cpload\t\\\$25\n" } } */
/* { dg-final { scan-assembler "\t\\.cprestore\t16\n" } } */
/* { dg-final { scan-assembler "\tlw\t\\\$1,16\\(\\\$(fp|sp)\\)\n" } } */
/* { dg-final { scan-assembler "\tlw\t\\\$1,%got\\(\[^)\]*\\)\\(\\\$1\\)\n" } } */
/* { dg-final { scan-assembler "\taddiu\t\\\$1,\\\$1,%lo\\(\[^)\]*\\)\n" } } */
/* { dg-final { scan-assembler "\tjr\t\\\$1\n" } } */
/* { dg-final { scan-assembler-not "\\\$28" } } */

#include "branch-helper.h"

NOMIPS16 void
foo (int (*bar) (void), int *x)
{
  *x = bar ();
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x10000;
}
