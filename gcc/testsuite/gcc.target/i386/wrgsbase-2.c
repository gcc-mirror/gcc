/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "wrgsbase\[ \t]+(%|)rdi" } } */

#include <immintrin.h>

void
write_gs_base64 (unsigned long long base)
{
  _writegsbase_u64 (base);
}
