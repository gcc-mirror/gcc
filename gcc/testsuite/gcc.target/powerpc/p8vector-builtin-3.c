/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -ftree-vectorize -fvect-cost-model=dynamic" } */

#include <altivec.h>

typedef vector long long		vll_sign;
typedef vector unsigned long long	vll_uns;
typedef vector bool long long		vll_bool;

typedef vector int			vi_sign;
typedef vector unsigned int		vi_uns;
typedef vector bool int			vi_bool;

typedef vector short			vs_sign;
typedef vector unsigned short		vs_uns;
typedef vector bool short		vs_bool;

typedef vector signed char		vc_sign;
typedef vector unsigned char		vc_uns;
typedef vector bool char		vc_bool;


vi_sign vi_pack_1 (vll_sign a, vll_sign b)
{
  return __builtin_altivec_vpkudum (a, b);
}

vi_sign vi_pack_2 (vll_sign a, vll_sign b)
{
  return vec_pack (a, b);
}

vi_uns vi_pack_3 (vll_uns a, vll_uns b)
{
  return vec_pack (a, b);
}

vi_sign vi_pack_4 (vll_sign a, vll_sign b)
{
  return vec_vpkudum (a, b);
}

vs_sign vs_pack_1 (vi_sign a, vi_sign b)
{
  return __builtin_altivec_vpkuwum (a, b);
}

vs_sign vs_pack_2 (vi_sign a, vi_sign b)
{
  return vec_pack (a, b);
}

vs_sign vs_pack_3 (vi_sign a, vi_sign b)
{
  return vec_vpkuwum (a, b);
}

vc_sign vc_pack_1 (vs_sign a, vs_sign b)
{
  return __builtin_altivec_vpkuhum (a, b);
}

vc_sign vc_pack_2 (vs_sign a, vs_sign b)
{
  return vec_pack (a, b);
}

vc_sign vc_pack_3 (vs_sign a, vs_sign b)
{
  return vec_vpkuhum (a, b);
}

vll_sign vll_unpack_hi_1 (vi_sign a)
{
  return __builtin_altivec_vupkhsw (a);
}

vll_sign vll_unpack_hi_2 (vi_sign a)
{
  return vec_unpackh (a);
}

vll_sign vll_unpack_hi_3 (vi_sign a)
{
  return __builtin_vec_vupkhsw (a);
}

vll_sign vll_unpack_lo_1 (vi_sign a)
{
  return vec_vupklsw (a);
}

vll_sign vll_unpack_lo_2 (vi_sign a)
{
  return vec_unpackl (a);
}

vll_sign vll_unpack_lo_3 (vi_sign a)
{
  return vec_vupklsw (a);
}

/* { dg-final { scan-assembler-times "vpkudum" 4 } } */
/* { dg-final { scan-assembler-times "vpkuwum" 3 } } */
/* { dg-final { scan-assembler-times "vpkuhum" 3 } } */
/* { dg-final { scan-assembler-times "vupklsw" 3 } } */
/* { dg-final { scan-assembler-times "vupkhsw" 3 } } */
