/* { dg-do link } */
/* { dg-options "-O2 -flto -shared -fPIC --save-temps" } */
/* { dg-require-effective-target shared } */
/* { dg-require-effective-target fpic } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

svint32_t
add1 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b8 (), x, y);
}

svint32_t
add2 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b16 (), x, y);
}

svint32_t
add3 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b32 (), x, y);
}

svint32_t
add4 (svint32_t x, svint32_t y)
{
  return svadd_z (svptrue_b64 (), x, y);
}

svint32_t
add5 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b8 (), x, y);
}

svint32_t
add6 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b16 (), x, y);
}

svint32_t
add7 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b32 (), x, y);
}

svint32_t
add8 (svint32_t x, svint32_t y)
{
  return svadd_m (svptrue_b64 (), x, y);
}

svint16_t
add9 (svint16_t x, svint16_t y)
{
  return svadd_m (svptrue_b32 (), x, y);
}

svint32_t
and1 (svint32_t x)
{
  return svand_z (svptrue_b8 (), x, 1);
}

svint32_t
and2 (svint32_t x)
{
  return svand_z (svptrue_b16 (), x, 1);
}

svint32_t
and3 (svint32_t x)
{
  return svand_z (svptrue_b32 (), x, 1);
}

svint32_t
and4 (svint32_t x)
{
  return svand_z (svptrue_b64 (), x, 1);
}

svint32_t
and5 (svint32_t x)
{
  return svand_m (svptrue_b8 (), x, 1);
}

svint32_t
and6 (svint32_t x)
{
  return svand_m (svptrue_b16 (), x, 1);
}

svint32_t
and7 (svint32_t x)
{
  return svand_m (svptrue_b32 (), x, 1);
}

svint32_t
and8 (svint32_t x)
{
  return svand_m (svptrue_b64 (), x, 1);
}

svbool_t
and9 (svbool_t x, svbool_t y)
{
  return svand_z (svptrue_b8 (), x, y);
}

svint32_t
not1 (svint32_t x, svint32_t y)
{
  return svnot_m (x, svptrue_b8 (), y);
}

svint32_t
cvt1 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b8 (), x);
}

svint32_t
cvt2 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b16 (), x);
}

svint32_t
cvt3 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b32 (), x);
}

svint32_t
cvt4 (svfloat16_t x)
{
  return svcvt_s32_z (svptrue_b64 (), x);
}

svfloat16_t
cvt5 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b8 (), x);
}

svfloat16_t
cvt6 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b16 (), x);
}

svfloat16_t
cvt7 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b32 (), x);
}

svfloat16_t
cvt8 (svfloat32_t x)
{
  return svcvt_f16_z (svptrue_b64 (), x);
}

svfloat16_t
cvt9 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b8 (), x);
}

svfloat16_t
cvt10 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b16 (), x);
}

svfloat16_t
cvt11 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b32 (), x);
}

svfloat16_t
cvt12 (svint16_t x)
{
  return svcvt_f16_z (svptrue_b64 (), x);
}

#ifdef __cplusplus
}
#endif
