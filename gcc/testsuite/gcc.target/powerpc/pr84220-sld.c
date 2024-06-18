/* PR target/84220 */
/* Test to ensure we generate invalid parameter errors rather than an ICE
    when calling builtin_vec_sld() with invalid parameters.  */
/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

typedef vector bool char vbc_t;
typedef vector signed char vsc_t;
typedef vector unsigned char vuc_t;
typedef vector bool int vbi_t;
typedef vector signed int vsi_t;
typedef vector unsigned int vui_t;
typedef vector pixel vp_t;
typedef vector bool short vbs_t;
typedef vector signed short vss_t;
typedef vector unsigned short vus_t;
typedef vector float vf_t;

void 
test_vbc ( vbc_t v1, vbc_t v2, vbc_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vsc ( vsc_t v1, vsc_t v2, vsc_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vuc ( vuc_t v1, vuc_t v2, vuc_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vbi ( vbi_t v1, vbi_t v2, vbi_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vsi ( vsi_t v1, vsi_t v2, vsi_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vui ( vui_t v1, vui_t v2, vui_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vp ( vp_t v1, vp_t v2, vp_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vbs ( vbs_t v1, vbs_t v2, vbs_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vss ( vss_t v1, vss_t v2, vss_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vus ( vus_t v1, vus_t v2, vus_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vf ( vf_t v1, vf_t v2, vf_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}
