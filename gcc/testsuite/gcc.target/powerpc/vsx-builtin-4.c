/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "xvcmpeqdp." } } */
/* { dg-final { scan-assembler "xvcmpgtdp." } } */
/* { dg-final { scan-assembler "xvcmpgedp." } } */
/* { dg-final { scan-assembler "xvcmpeqsp." } } */
/* { dg-final { scan-assembler "xvcmpgtsp." } } */
/* { dg-final { scan-assembler "xvcmpgesp." } } */
/* { dg-final { scan-assembler "vcmpbfp." } } */
/* { dg-final { scan-assembler "vcmpequb." } } */
/* { dg-final { scan-assembler "vcmpequh." } } */
/* { dg-final { scan-assembler "vcmpequw." } } */
/* { dg-final { scan-assembler "vcmpgtub." } } */
/* { dg-final { scan-assembler "vcmpgtuh." } } */
/* { dg-final { scan-assembler "vcmpgtuw." } } */
/* { dg-final { scan-assembler "vcmpgtsb." } } */
/* { dg-final { scan-assembler "vcmpgtsh." } } */
/* { dg-final { scan-assembler "vcmpgtsw." } } */
/* { dg-final { scan-assembler-not "vcmpeqfp" } } */
/* { dg-final { scan-assembler-not "vcmpgtfp" } } */
/* { dg-final { scan-assembler-not "vcmpgefp" } } */

/* check that Altivec builtins generate VSX if -mvsx.  */

#include <altivec.h>

int *v16qi_s (vector signed char *a, vector signed char *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v16qi_u (vector unsigned char *a, vector unsigned char *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v8hi_s (vector short *a, vector short *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v8hi_u (vector unsigned short *a, vector unsigned short *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v4si_s (vector int *a, vector int *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v4si_u (vector unsigned int *a, vector unsigned int *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 2;

  return p;
}

int *v4sf (vector float *a, vector float *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 3;

  if (vec_all_in (*a, *b))	/* veccmpbfp. */
    *p++ = 4;

  return p;
}

int *v2df (vector double *a, vector double *b, int *p)
{
  if (vec_all_eq (*a, *b))
    *p++ = 1;

  if (vec_all_gt (*a, *b))
    *p++ = 2;

  if (vec_all_ge (*a, *b))
    *p++ = 3;

  return p;
}
