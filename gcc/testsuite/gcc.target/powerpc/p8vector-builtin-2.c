/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#include <altivec.h>

typedef vector long long		v_sign;
typedef vector unsigned long long	v_uns;
typedef vector bool long long		v_bool;

v_sign sign_add_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vaddudm (a, b);
}

v_sign sign_add_2 (v_sign a, v_sign b)
{
  return vec_add (a, b);
}

v_sign sign_add_3 (v_sign a, v_sign b)
{
  return vec_vaddudm (a, b);
}

v_sign sign_sub_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vsubudm (a, b);
}

v_sign sign_sub_2 (v_sign a, v_sign b)
{
  return vec_sub (a, b);
}


v_sign sign_sub_3 (v_sign a, v_sign b)
{
  return vec_vsubudm (a, b);
}

v_sign sign_min_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vminsd (a, b);
}

v_sign sign_min_2 (v_sign a, v_sign b)
{
  return vec_min (a, b);
}

v_sign sign_min_3 (v_sign a, v_sign b)
{
  return vec_vminsd (a, b);
}

v_sign sign_max_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vmaxsd (a, b);
}

v_sign sign_max_2 (v_sign a, v_sign b)
{
  return vec_max (a, b);
}

v_sign sign_max_3 (v_sign a, v_sign b)
{
  return vec_vmaxsd (a, b);
}

v_sign sign_abs (v_sign a)
{
  return vec_abs (a);		/* xor, vsubudm, vmaxsd.  */
}

v_bool sign_eq (v_sign a, v_sign b)
{
  return vec_cmpeq (a, b);
}

v_bool sign_lt (v_sign a, v_sign b)
{
  return vec_cmplt (a, b);
}

v_uns uns_add_2 (v_uns a, v_uns b)
{
  return vec_add (a, b);
}

v_uns uns_add_3 (v_uns a, v_uns b)
{
  return vec_vaddudm (a, b);
}

v_uns uns_sub_2 (v_uns a, v_uns b)
{
  return vec_sub (a, b);
}

v_uns uns_sub_3 (v_uns a, v_uns b)
{
  return vec_vsubudm (a, b);
}

v_uns uns_min_2 (v_uns a, v_uns b)
{
  return vec_min (a, b);
}

v_uns uns_min_3 (v_uns a, v_uns b)
{
  return vec_vminud (a, b);
}

v_uns uns_max_2 (v_uns a, v_uns b)
{
  return vec_max (a, b);
}

v_uns uns_max_3 (v_uns a, v_uns b)
{
  return vec_vmaxud (a, b);
}

v_bool uns_eq (v_uns a, v_uns b)
{
  return vec_cmpeq (a, b);
}

v_bool uns_lt (v_uns a, v_uns b)
{
  return vec_cmplt (a, b);
}

v_sign sign_rl_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vrld (a, b);
}

v_sign sign_rl_2 (v_sign a, v_uns b)
{
  return vec_rl (a, b);
}

v_uns uns_rl_2 (v_uns a, v_uns b)
{
  return vec_rl (a, b);
}

v_sign sign_sl_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vsld (a, b);
}

v_sign sign_sl_2 (v_sign a, v_uns b)
{
  return vec_sl (a, b);
}

v_sign sign_sl_3 (v_sign a, v_uns b)
{
  return vec_vsld (a, b);
}

v_uns uns_sl_2 (v_uns a, v_uns b)
{
  return vec_sl (a, b);
}

v_uns uns_sl_3 (v_uns a, v_uns b)
{
  return vec_vsld (a, b);
}

v_sign sign_sra_1 (v_sign a, v_sign b)
{
  return __builtin_altivec_vsrad (a, b);
}

v_sign sign_sra_2 (v_sign a, v_uns b)
{
  return vec_sra (a, b);
}

v_sign sign_sra_3 (v_sign a, v_uns b)
{
  return vec_vsrad (a, b);
}

/* { dg-final { scan-assembler-times "vaddudm" 	5 } } */
/* { dg-final { scan-assembler-times "vsubudm" 	6 } } */
/* { dg-final { scan-assembler-times "vmaxsd"  	4 } } */
/* { dg-final { scan-assembler-times "vminsd"  	3 } } */
/* { dg-final { scan-assembler-times "vmaxud"  	2 } } */
/* { dg-final { scan-assembler-times "vminud"  	2 } } */
/* { dg-final { scan-assembler-times "vcmpequd" 2 } } */
/* { dg-final { scan-assembler-times "vcmpgtsd" 1 } } */
/* { dg-final { scan-assembler-times "vcmpgtud" 1 } } */
/* { dg-final { scan-assembler-times "vrld"     3 } } */
/* { dg-final { scan-assembler-times "vsld"     5 } } */
/* { dg-final { scan-assembler-times "vsrad"    3 } } */
