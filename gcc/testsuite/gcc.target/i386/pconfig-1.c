/* { dg-do compile } */
/* { dg-options "-O2 -mpconfig" } */
/* { dg-final { scan-assembler-times {\tpconfig} 2 } } */

#include <x86intrin.h>

extern unsigned int leaf;

#define PCONFIG_KEY_PROGRAM 0x01

int test ()
{
  size_t D[3] = {1, 2, 3};

  unsigned int res1 = _pconfig_u32 (leaf, D);

  unsigned int res2 = _pconfig_u32 (PCONFIG_KEY_PROGRAM, D);

  return 0;
}
