/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */
/* { dg-final { scan-assembler-times "xxsel" 6 } } */

#include <altivec.h>

/* Signed args */
vector signed __int128
test_vec_sel_ssb (vector signed __int128 src_va_s128,
		  vector signed __int128 src_vb_s128,
		  vector bool __int128 src_vc_b128)
{
  return vec_sel (src_va_s128, src_vb_s128, src_vc_b128);
}

vector signed __int128
test_vec_sel_ssu (vector signed __int128 src_va_s128,
		  vector signed __int128 src_vb_s128,
		  vector unsigned __int128 src_vc_u128)
{
  return vec_sel (src_va_s128, src_vb_s128, src_vc_u128);
}

/* Unsigned args */
vector unsigned __int128
test_vec_sel_uub (vector unsigned __int128 src_va_u128,
		  vector unsigned __int128 src_vb_u128,
		  vector bool __int128 src_vc_b128)
{
  return vec_sel (src_va_u128, src_vb_u128, src_vc_b128);
}

vector unsigned __int128
test_vec_sel_uuu (vector unsigned __int128 src_va_u128,
		  vector unsigned __int128 src_vb_u128,
		  vector unsigned __int128 src_vc_u128)
{
  return vec_sel (src_va_u128, src_vb_u128, src_vc_u128);
}

/* Boolean args */
vector bool __int128
test_vec_sel_bbb (vector bool __int128 src_va_b128,
		  vector bool __int128 src_vb_b128,
		  vector bool __int128 src_vc_b128)
{
  return vec_sel (src_va_b128, src_vb_b128, src_vc_b128);
}

vector bool __int128
test_vec_sel_bbu (vector bool __int128 src_va_b128,
		  vector bool __int128 src_vb_b128,
		  vector unsigned __int128 src_vc_u128)
{
  return vec_sel (src_va_b128, src_vb_b128, src_vc_u128);
}

/* Expected results:
   vec_sel              xxsel    */

/* { dg-final { scan-assembler-times "xxsel" 6 } } */

