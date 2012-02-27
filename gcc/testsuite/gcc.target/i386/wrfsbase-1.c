/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "wrfsbase\[ \t]+(%|)(edi|ecx)" } } */

#include <immintrin.h>

void
write_fs_base32 (unsigned int base)
{
  _writefsbase_u32 (base);
}
