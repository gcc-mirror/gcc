/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

#ifdef __LITTLE_ENDIAN__
#define ELEMENT 1
#else
#define ELEMENT 0
#endif

void foo (double *p, vector double v)
{
  p[10] = vec_extract (v, ELEMENT);
}

/* { dg-final { scan-assembler     "stxsd "   } } */
/* { dg-final { scan-assembler-not "stxsdx"   } } */
/* { dg-final { scan-assembler-not "stfd"     } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
