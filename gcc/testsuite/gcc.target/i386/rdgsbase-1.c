/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mfsgsbase" } */
/* { dg-final { scan-assembler "rdgsbase\[ \t]+(%|)eax" } } */

#include <immintrin.h>

unsigned int
read_gs_base32 (void)
{
  return _readgsbase_u32 ();
}
