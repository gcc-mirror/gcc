/* PR target/84220 */
/* Test to ensure we generate invalid parameter errors rather than an ICE
    when calling builtin_vec_sldw() with invalid parameters.  */
/* { dg-do compile } */
/* { dg-options "-mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

typedef  vector bool char vbc_t;
typedef  vector  signed char vsc_t;
typedef  vector  unsigned char vuc_t;
typedef  vector  bool int vbi_t;
typedef  vector  signed int vsi_t;
typedef  vector  unsigned int vui_t;
typedef  vector  pixel vp_t;
typedef  vector  bool short vbs_t;
typedef  vector  signed short vss_t;
typedef  vector  unsigned short vus_t;
typedef  vector  float vf_t;
typedef  vector  bool long long vbl_t;
typedef  vector  signed long long vsl_t;
typedef  vector  unsigned long long vul_t;
typedef  vector  double vd_t;

void 
test_vsc ( vsc_t v1, vsc_t v2, vsc_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vuc ( vuc_t v1, vuc_t v2, vuc_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vsi ( vsi_t v1, vsi_t v2, vsi_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vui ( vui_t v1, vui_t v2, vui_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vsl ( vsl_t v1, vsl_t v2, vsl_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vul ( vul_t v1, vul_t v2, vul_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vss ( vss_t v1, vss_t v2, vss_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}

void 
test_vus ( vus_t v1, vus_t v2, vus_t v3 )  \
{
  vec_sldw(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  vec_sldw(v1, v2, 3);
}
