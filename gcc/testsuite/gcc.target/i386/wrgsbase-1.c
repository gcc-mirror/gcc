/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "wrgsbase\[ \t]+(%|)edi" } } */

#include <immintrin.h>

void
write_gs_base32 (unsigned int base)
{
  _writegsbase_u32 (base);
}
