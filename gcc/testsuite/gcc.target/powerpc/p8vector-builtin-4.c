/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O3 -ftree-vectorize -fvect-cost-model" } */

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

vll_sign vll_clz_1 (vll_sign a)
{
  return __builtin_altivec_vclzd (a);
}

vll_sign vll_clz_2 (vll_sign a)
{
  return vec_vclz (a);
}

vll_sign vll_clz_3 (vll_sign a)
{
  return vec_vclzd (a);
}

vll_uns vll_clz_4 (vll_uns a)
{
  return vec_vclz (a);
}

vll_uns vll_clz_5 (vll_uns a)
{
  return vec_vclzd (a);
}

vi_sign vi_clz_1 (vi_sign a)
{
  return __builtin_altivec_vclzw (a);
}

vi_sign vi_clz_2 (vi_sign a)
{
  return vec_vclz (a);
}

vi_sign vi_clz_3 (vi_sign a)
{
  return vec_vclzw (a);
}

vi_uns vi_clz_4 (vi_uns a)
{
  return vec_vclz (a);
}

vi_uns vi_clz_5 (vi_uns a)
{
  return vec_vclzw (a);
}

vs_sign vs_clz_1 (vs_sign a)
{
  return __builtin_altivec_vclzh (a);
}

vs_sign vs_clz_2 (vs_sign a)
{
  return vec_vclz (a);
}

vs_sign vs_clz_3 (vs_sign a)
{
  return vec_vclzh (a);
}

vs_uns vs_clz_4 (vs_uns a)
{
  return vec_vclz (a);
}

vs_uns vs_clz_5 (vs_uns a)
{
  return vec_vclzh (a);
}

vc_sign vc_clz_1 (vc_sign a)
{
  return __builtin_altivec_vclzb (a);
}

vc_sign vc_clz_2 (vc_sign a)
{
  return vec_vclz (a);
}

vc_sign vc_clz_3 (vc_sign a)
{
  return vec_vclzb (a);
}

vc_uns vc_clz_4 (vc_uns a)
{
  return vec_vclz (a);
}

vc_uns vc_clz_5 (vc_uns a)
{
  return vec_vclzb (a);
}

vll_sign vll_popcnt_1 (vll_sign a)
{
  return __builtin_altivec_vpopcntd (a);
}

vll_sign vll_popcnt_2 (vll_sign a)
{
  return vec_vpopcnt (a);
}

vll_sign vll_popcnt_3 (vll_sign a)
{
  return vec_vpopcntd (a);
}

vll_uns vll_popcnt_4 (vll_uns a)
{
  return vec_vpopcnt (a);
}

vll_uns vll_popcnt_5 (vll_uns a)
{
  return vec_vpopcntd (a);
}

vi_sign vi_popcnt_1 (vi_sign a)
{
  return __builtin_altivec_vpopcntw (a);
}

vi_sign vi_popcnt_2 (vi_sign a)
{
  return vec_vpopcnt (a);
}

vi_sign vi_popcnt_3 (vi_sign a)
{
  return vec_vpopcntw (a);
}

vi_uns vi_popcnt_4 (vi_uns a)
{
  return vec_vpopcnt (a);
}

vi_uns vi_popcnt_5 (vi_uns a)
{
  return vec_vpopcntw (a);
}

vs_sign vs_popcnt_1 (vs_sign a)
{
  return __builtin_altivec_vpopcnth (a);
}

vs_sign vs_popcnt_2 (vs_sign a)
{
  return vec_vpopcnt (a);
}

vs_sign vs_popcnt_3 (vs_sign a)
{
  return vec_vpopcnth (a);
}

vs_uns vs_popcnt_4 (vs_uns a)
{
  return vec_vpopcnt (a);
}

vs_uns vs_popcnt_5 (vs_uns a)
{
  return vec_vpopcnth (a);
}

vc_sign vc_popcnt_1 (vc_sign a)
{
  return __builtin_altivec_vpopcntb (a);
}

vc_sign vc_popcnt_2 (vc_sign a)
{
  return vec_vpopcnt (a);
}

vc_sign vc_popcnt_3 (vc_sign a)
{
  return vec_vpopcntb (a);
}

vc_uns vc_popcnt_4 (vc_uns a)
{
  return vec_vpopcnt (a);
}

vc_uns vc_popcnt_5 (vc_uns a)
{
  return vec_vpopcntb (a);
}

vc_uns vc_gbb_1 (vc_uns a)
{
  return __builtin_altivec_vgbbd (a);
}

vc_sign vc_gbb_2 (vc_sign a)
{
  return vec_vgbbd (a);
}

vc_uns vc_gbb_3 (vc_uns a)
{
  return vec_vgbbd (a);
}

/* { dg-final { scan-assembler-times "vclzd" 	5 } } */
/* { dg-final { scan-assembler-times "vclzw" 	5 } } */
/* { dg-final { scan-assembler-times "vclzh" 	5 } } */
/* { dg-final { scan-assembler-times "vclzb" 	5 } } */

/* { dg-final { scan-assembler-times "vpopcntd" 5 } } */
/* { dg-final { scan-assembler-times "vpopcntw" 5 } } */
/* { dg-final { scan-assembler-times "vpopcnth" 5 } } */
/* { dg-final { scan-assembler-times "vpopcntb" 5 } } */

/* { dg-final { scan-assembler-times "vgbbd"    3 } } */
