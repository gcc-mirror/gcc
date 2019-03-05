/* { dg-do compile } */
/* { dg-options "-O2 -mptwrite " } */
/* { dg-final { scan-assembler "ptwrite\[^\n\r\]+r" } } */
/* { dg-final { scan-assembler "ptwrite\[^\n\r\]+e" } } */

#include <x86intrin.h>

void ptwrite1(void)
{
  _ptwrite32 (1);
#ifdef __x86_64__
  _ptwrite64 (2);
#endif
}
