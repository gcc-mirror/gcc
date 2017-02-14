/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power8" } */

#include <altivec.h>

short
add_short_0 (vector short *p)
{
  return vec_extract (*p, 0) + 1;
}

short
add_short_1 (vector short *p)
{
  return vec_extract (*p, 1) + 1;
}

short
add_short_2 (vector short *p)
{
  return vec_extract (*p, 2) + 1;
}

short
add_short_3 (vector short *p)
{
  return vec_extract (*p, 3) + 1;
}

short
add_short_4 (vector short *p)
{
  return vec_extract (*p, 4) + 1;
}

short
add_short_5 (vector short *p)
{
  return vec_extract (*p, 5) + 1;
}

short
add_short_6 (vector short *p)
{
  return vec_extract (*p, 6) + 1;
}

short
add_short_7 (vector short *p)
{
  return vec_extract (*p, 7) + 1;
}

short
add_short_n (vector short *p, int n)
{
  return vec_extract (*p, n) + 1;
}

/* { dg-final { scan-assembler-not "lxvd2x"   } } */
/* { dg-final { scan-assembler-not "lxvw4x"   } } */
/* { dg-final { scan-assembler-not "lxvx"     } } */
/* { dg-final { scan-assembler-not "lxv"      } } */
/* { dg-final { scan-assembler-not "lvx"      } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */
