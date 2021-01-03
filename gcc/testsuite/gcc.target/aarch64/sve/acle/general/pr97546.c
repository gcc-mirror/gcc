/* { dg-options "-O2" } */

#include <arm_sve.h>

static svbool_t visinf_vo_vf(svfloat32_t d)
{
  return svcmpeq_n_f32 (svptrue_b8 (),
                        svabs_f32_x (svptrue_b8 (), d),
                        __builtin_inff ());
}

const svint32_t _ZGVsNxv_ilogbf(svfloat32_t d)
{
  svint32_t e = svreinterpret_s32_f32 (svdup_n_f32 (0.0f));
  e = svsel_s32 (svcmpne_f32 (svptrue_b8(), d, d),
                 svdup_n_s32 (2147483647),
                 e);
  e = svsel_s32 (visinf_vo_vf (d),
                 svdup_n_s32 (0x7fffffff),
                 e);
  return e;
}
