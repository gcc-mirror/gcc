/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

typedef __vector unsigned __int128 vui128_t;

typedef union
{
  __float128 vf1;
  vui128_t vx1;
} __VF_128;

vui128_t
vec_xfer_bin128_2_vui128t (__float128 f128)
{
  __VF_128 vunion;
  vunion.vf1 = f128;
  return (vunion.vx1);
}

/* { dg-final { scan-assembler-not {\mxxpermdi\M} } } */
/* { dg-final { scan-assembler-not {\mstxvd2x\M} } } */
/* { dg-final { scan-assembler-not {\mlxvd2x\M} } } */

