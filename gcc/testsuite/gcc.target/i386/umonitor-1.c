/* { dg-do compile } */
/* { dg-options "-O2 -mwaitpkg" } */
/* { dg-final { scan-assembler-times "umonitor\[ \\t\]+\[^\{\n\]*%"  3 } } */
/* { dg-final { scan-assembler-times "umwait"  3 } } */
/* { dg-final { scan-assembler-times "setc\[ \\t\]+\[^\{\n\]*%" 3 } } */

/* Verify that they work in both 32bit and 64bit.  */

#include <x86intrin.h>

unsigned char
foo (void *p, unsigned x, unsigned y)
{
   _umonitor (p);
   return _umwait (x, y);
}

unsigned char
bar (void *p, unsigned x, unsigned long long y)
{
   _umonitor (p);
   return _umwait (x, y);
}

unsigned char
foo1 (char *p)
{
   _umonitor (p);
   return _umwait (0, 0);
}
