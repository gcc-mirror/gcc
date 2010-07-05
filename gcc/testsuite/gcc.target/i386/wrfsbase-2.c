/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "wrfsbase\[ \t]+(%|)rdi" } } */

#include <immintrin.h>

void
write_fs_base64 (unsigned long long base)
{
  _writefsbase_u64 (base);
}
