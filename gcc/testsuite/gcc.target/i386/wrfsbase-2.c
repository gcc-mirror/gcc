/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "wrfsbase\[ \t]+(%|)(rdi|rcx)" } } */

#include <immintrin.h>

void
write_fs_base64 (unsigned long long base)
{
  _writefsbase_u64 (base);
}
