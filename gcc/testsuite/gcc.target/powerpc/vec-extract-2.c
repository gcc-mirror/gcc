/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

#include <altivec.h>

double
add_double_0 (vector double *p, double x)
{
  return vec_extract (*p, 0) + x;
}

double
add_double_1 (vector double *p, double x)
{
  return vec_extract (*p, 1) + x;
}

long
add_long_0 (vector long *p, long x)
{
  return vec_extract (*p, 0) + x;
}

long
add_long_1 (vector long *p, long x)
{
  return vec_extract (*p, 1) + x;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */
/* { dg-final { scan-assembler-not "lvx"      } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
