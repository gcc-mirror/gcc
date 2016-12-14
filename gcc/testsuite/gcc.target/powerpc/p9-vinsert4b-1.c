/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

#include <altivec.h>

vector signed char
vins_v4si (vector int *vi, vector signed char *vc)
{
  return vec_vinsert4b (*vi, *vc, 1);
}

vector unsigned char
vins_di (long di, vector unsigned char *vc)
{
  return vec_vinsert4b (di, *vc, 2);
}

vector char
vins_di2 (long *p_di, vector char *vc)
{
  return vec_vinsert4b (*p_di, *vc, 3);
}

vector unsigned char
vins_di0 (vector unsigned char *vc)
{
  return vec_vinsert4b (0, *vc, 4);
}

long
vext (vector signed char *vc)
{
  return vec_vextract4b (*vc, 5);
}

/* { dg-final { scan-assembler "xxextractuw\|vextuw\[lr\]x" } } */
/* { dg-final { scan-assembler "xxinsertw" } } */
