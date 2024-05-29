/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
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
