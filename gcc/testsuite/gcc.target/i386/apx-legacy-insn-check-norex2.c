/* { dg-do compile } */
/* { dg-options "-O3 -mapxf -m64 -DDTYPE32" } */

#include <immintrin.h>

typedef unsigned int u32;
typedef unsigned long long u64;

#ifndef DTYPE32
#define DTYPE32
#endif

#ifdef DTYPE32
typedef u32 DTYPE;
#endif

__attribute__((target("xsave,fxsr")))
void legacy_test ()
{
  register DTYPE* val __asm__("r16");
  _xsave64 (val, 1);
  _xrstor64 (val, 1);
  _fxsave64 (val);
  _fxrstor64 (val);
}

/* { dg-final { scan-assembler-not "xsave64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "xrstor64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "fxsave64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
/* { dg-final { scan-assembler-not "fxrstor64\[ \\t]+\\\.\\\*r\(1\[6-9\]\|2\[0-9\]|30\|31\)" } } */
