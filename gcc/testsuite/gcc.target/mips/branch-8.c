/* { dg-options "-mshared -mabi=32" } */
/* { dg-final { scan-assembler-not "(\\\$28|cpload|cprestore)" } } */
/* { dg-final { scan-assembler-not "\tjrc?\t\\\$1\n" } } */

#include "branch-helper.h"

NOCOMPRESSION void
foo (int (*bar) (void), int *x)
{
  *x = bar ();
  if (__builtin_expect (*x == 0, 1))
    OCCUPY_0x1fff8;
}
