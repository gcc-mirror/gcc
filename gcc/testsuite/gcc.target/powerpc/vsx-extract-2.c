/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7" } */
/* { dg-final { scan-assembler     "xxlor"  } } */
/* { dg-final { scan-assembler-not "lfd"    } } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */

#include <altivec.h>

#if __LITTLE_ENDIAN__
#define OFFSET 1
#else
#define OFFSET 0
#endif

double get_value (vector double v) { return vec_extract (v, OFFSET); }
