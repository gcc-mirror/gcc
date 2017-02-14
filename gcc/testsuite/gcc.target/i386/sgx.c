/* { dg-do compile } */
/* { dg-options "-O2 -msgx" } */
/* { dg-final { scan-assembler-times "enclu"  2 } } */
/* { dg-final { scan-assembler-times "encls"  2 } } */

#include <x86intrin.h>

extern int leaf;

#define SGX_EENTER 0x02
#define SGX_EBLOCK 0x09

int foo ()
{
  size_t test[3];
  test[0] = 4;
  test[1] = 5;
  test[2] = 6; 
  int res1 = _encls_u32 (leaf, test);
  int res2 = _enclu_u32 (leaf, test);
  int res3 = _encls_u32 (SGX_EBLOCK, test);
  int res4 = _enclu_u32 (SGX_EENTER, test);
  return 0;
}
