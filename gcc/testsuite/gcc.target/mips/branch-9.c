/* { dg-options "-mabicalls -mshared -mabi=32" } */
/* { dg-final { scan-assembler "\t\\.cpload\t\\\$25\n" } } */
/* { dg-final { scan-assembler "\t\\.cprestore\t16\n" } } */
/* { dg-final { scan-assembler "\tlw\t\\\$1,16\\(\\\$fp\\)\n" } } */
/* { dg-final { scan-assembler "\tlw\t\\\$1,%got\\(\[^)\]*\\)\\(\\\$1\\)\n" } } */
/* { dg-final { scan-assembler "\taddiu\t\\\$1,\\\$1,%lo\\(\[^)\]*\\)\n" } } */
/* { dg-final { scan-assembler "\tjr\t\\\$1\n" } } */
/* { dg-final { scan-assembler-not "\tlw\t\\\$28,16\\(\\\$sp\\)\n" } } */

#include "branch-helper.h"

NOMIPS16 void
foo (void (*bar) (void), volatile int *x)
{
  bar ();
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fffc;
}
