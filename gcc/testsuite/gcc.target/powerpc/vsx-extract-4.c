/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power8" } */

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
  int i = vec_extract (v, 0);
  return (TYPE) i;
}

TYPE
foo_1s (vector int v)
{
  int i = vec_extract (v, 1);
  return (TYPE) i;
}

TYPE
foo_2s (vector int v)
{
  int i = vec_extract (v, 2);
  return (TYPE) i;
}

TYPE
foo_3s (vector int v)
{
  int i = vec_extract (v, 3);
  return (TYPE) i;
}

TYPE
foo_0u (vector unsigned int v)
{
  unsigned int u = vec_extract (v, 0);
  return (TYPE) u;
}

TYPE
foo_1u (vector unsigned int v)
{
  unsigned int u = vec_extract (v, 1);
  return (TYPE) u;
}

TYPE
foo_2u (vector unsigned int v)
{
  unsigned int u = vec_extract (v, 2);
  return (TYPE) u;
}

TYPE
foo_3u (vector unsigned int v)
{
  unsigned int u = vec_extract (v, 3);
  return (TYPE) u;
}
