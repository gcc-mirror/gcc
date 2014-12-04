/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O3 -mcpu=power8" } */
/* { dg-final { scan-assembler     "vbpermq" } } */
/* { dg-final { scan-assembler     "mfvsrd"  } } */
/* { dg-final { scan-assembler-not "stfd"    } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */

#include <altivec.h>

#if __LITTLE_ENDIAN__
#define OFFSET 1
#else
#define OFFSET 0
#endif

long foos (vector signed char a, vector signed char b)
{
  return vec_extract (vec_vbpermq (a, b), OFFSET);
}

long foou (vector unsigned char a, vector unsigned char b)
{
  return vec_extract (vec_vbpermq (a, b), OFFSET);
}

