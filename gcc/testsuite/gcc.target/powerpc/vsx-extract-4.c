/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */

/* { dg-final { scan-assembler-times "vspltw"    6 } } */
/* { dg-final { scan-assembler-times "xvcvsxwdp" 4 } } */
/* { dg-final { scan-assembler-times "xvcvuxwdp" 4 } } */
/* { dg-final { scan-assembler-not   "mtvsrd"      } } */
/* { dg-final { scan-assembler-not   "mtvsrwa"     } } */
/* { dg-final { scan-assembler-not   "mtvsrwz"     } } */
/* { dg-final { scan-assembler-not   "mfvsrd"      } } */
/* { dg-final { scan-assembler-not   "mfvsrwz"     } } */

#include <altivec.h>

#ifndef TYPE
#define TYPE double
#endif

TYPE
foo_0s (vector int v)
{
  int c = 0;
  int i = vec_extract (v, c);
  return (TYPE) i;
}

TYPE
foo_1s (vector int v)
{
  int c = 1;
  int i = vec_extract (v, c);
  return (TYPE) i;
}

TYPE
foo_2s (vector int v)
{
  int c = 2;
  int i = vec_extract (v, c);
  return (TYPE) i;
}

TYPE
foo_3s (vector int v)
{
  int c = 3;
  int i = vec_extract (v, c);
  return (TYPE) i;
}

TYPE
foo_0u (vector unsigned int v)
{
  int c = 0;
  unsigned int u = vec_extract (v, c);
  return (TYPE) u;
}

TYPE
foo_1u (vector unsigned int v)
{
  int c = 1;
  unsigned int u = vec_extract (v, c);
  return (TYPE) u;
}

TYPE
foo_2u (vector unsigned int v)
{
  int c = 2;
  unsigned int u = vec_extract (v, c);
  return (TYPE) u;
}

TYPE
foo_3u (vector unsigned int v)
{
  int c = 3;
  unsigned int u = vec_extract (v, c);
  return (TYPE) u;
}
