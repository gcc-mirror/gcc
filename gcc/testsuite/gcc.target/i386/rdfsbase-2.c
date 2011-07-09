/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "rdfsbase\[ \t]+(%|)rax" } } */

#include <immintrin.h>

unsigned long long
read_fs_base64 (void)
{
  return _readfsbase_u64 ();
}
