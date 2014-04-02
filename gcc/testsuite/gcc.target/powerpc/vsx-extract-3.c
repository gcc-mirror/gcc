/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O3 -mcpu=power8" } */
/* { dg-final { scan-assembler     "mfvsrd"  } } */
/* { dg-final { scan-assembler-not "stfd"    } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */

#include <altivec.h>

#if __LITTLE_ENDIAN__
#define OFFSET 1
#else
#define OFFSET 0
#endif

long get_value (vector long v) { return vec_extract (v, OFFSET); }
