/* PR target/84220 */
/* Test to ensure we generate invalid parameter errors rather than an ICE
    when calling builtin_vec_sld() with invalid parameters.  */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

typedef  vector  bool long long vbl_t;
typedef  vector  signed long long vsl_t;
typedef  vector  unsigned long long vul_t;
typedef  vector  double vd_t;

void 
test_vbl ( vbl_t v1, vbl_t v2, vbl_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vsl ( vsl_t v1, vsl_t v2, vsl_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vul ( vul_t v1, vul_t v2, vul_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}

void 
test_vd ( vd_t v1, vd_t v2, vd_t v3 )  \
{
  __builtin_vec_sld(v1, v2, v3); /* { dg-error "invalid parameter combination for AltiVec intrinsic" } */
  __builtin_vec_sld(v1, v2, 3);
}
