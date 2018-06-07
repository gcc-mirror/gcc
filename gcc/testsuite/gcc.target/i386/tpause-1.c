/* { dg-do compile } */
/* { dg-options "-O2 -mwaitpkg" } */
/* { dg-final { scan-assembler-times "tpause\[ \\t\]+\[^\{\n\]*%" 3 } } */
/* { dg-final { scan-assembler-times "setc\[ \\t\]+\[^\{\n\]*%" 3 } } */

#include <x86intrin.h>

unsigned char
foo (unsigned x, unsigned y)
{
   return _tpause (x, y);
}

unsigned char
bar (unsigned x, unsigned long long y)
{
   return _tpause (x, y);
}

unsigned char
foo1 (void)
{
   return _tpause (0, 0);
}
