/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*le-*-* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power7" } */

/* Make sure that vec_mergel and vec_mergeh are supported for V2DF/V2DI types.  */
/* { dg-final { scan-assembler-times "xxpermdi" 4 } } */

#include <altivec.h>

void vec_high_v2df (vector double *a, vector double *b, vector double *c)
{
  *a = vec_mergeh (*b, *c);
}

void vec_low_v2df (vector double *a, vector double *b, vector double *c)
{
  *a = vec_mergel (*b, *c);
}

void vec_high_v2di (vector long long *a, vector long long *b, vector long long *c)
{
  *a = vec_mergeh (*b, *c);
}

void vec_low_v2di (vector long long *a, vector long long *b, vector long long *c)
{
  *a = vec_mergel (*b, *c);
}
