/* { dg-options "-mshared -mabi=n32" } */
/* { dg-final { scan-assembler "\taddiu\t\\\$3,\\\$3,%lo\\(%neg\\(%gp_rel\\(foo\\)\\)\\)\n" } } */
/* { dg-final { scan-assembler "\tlw\t\\\$1,%got_page\\(\[^)\]*\\)\\(\\\$3\\)\\n" } } */
/* { dg-final { scan-assembler "\tjrc?\t\\\$1\n" } } */
/* { dg-final { scan-assembler-not "\\\$28" } } */

#include "branch-helper.h"

NOCOMPRESSION void
foo (volatile int *x)
{
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fffc;
}
