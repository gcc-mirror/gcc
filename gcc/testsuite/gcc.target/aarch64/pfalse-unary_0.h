#include <arm_sve.h>
#include <stdbool.h>

#define M(F, RTY, TY)						\
  RTY F##_f (RTY inactive, TY op)				\
  {								\
    return sv##F (inactive, svpfalse_b (), op);			\
  }

#define XZI(F, RTY, TY)						\
  RTY F##_f (TY op)						\
  {								\
    return sv##F (svpfalse_b (), op);				\
  }

#define PRED_Z(F, RTY, TYPE, TY)				\
  XZI (F##_##TY##_z, RTY, sv##TYPE)				\

#define PRED_MZ(F, RTY, TYPE, TY)				\
  M (F##_##TY##_m, RTY, sv##TYPE)				\
  PRED_Z (F, RTY, TYPE, TY)

#define PRED_MXZ(F, RTY, TYPE, TY)				\
  XZI (F##_##TY##_x, RTY, sv##TYPE)				\
  PRED_MZ (F, RTY, TYPE, TY)

#define PRED_IMPLICIT(F, RTY, TYPE, TY)				\
  XZI (F##_##TY, RTY, sv##TYPE)

#define PRED_IMPLICITn(F, RTY, TYPE)				\
  XZI (F, RTY, sv##TYPE)

#define Q_INTEGER(F, P)						\
  PRED_##P (F, svuint8_t, uint8_t, u8)				\
  PRED_##P (F, svint8_t, int8_t, s8)

#define Q_INTEGER_SCALAR(F, P)					\
  PRED_##P (F, uint8_t, uint8_t, u8)				\
  PRED_##P (F, int8_t, int8_t, s8)

#define Q_INTEGER_SCALAR_WIDE(F, P)				\
  PRED_##P (F, uint64_t, uint8_t, u8)				\
  PRED_##P (F, int64_t, int8_t, s8)

#define H_INTEGER(F, P)						\
  PRED_##P (F, svuint16_t, uint16_t, u16)			\
  PRED_##P (F, svint16_t, int16_t, s16)

#define H_INTEGER_SCALAR(F, P)					\
  PRED_##P (F, uint16_t, uint16_t, u16)				\
  PRED_##P (F, int16_t, int16_t, s16)

#define H_INTEGER_SCALAR_WIDE(F, P)				\
  PRED_##P (F, uint64_t, uint16_t, u16)				\
  PRED_##P (F, int64_t, int16_t, s16)

#define S_INTEGER(F, P)						\
  PRED_##P (F, svuint32_t, uint32_t, u32)			\
  PRED_##P (F, svint32_t, int32_t, s32)

#define S_INTEGER_SCALAR(F, P)					\
  PRED_##P (F, uint32_t, uint32_t, u32)				\
  PRED_##P (F, int32_t, int32_t, s32)

#define S_INTEGER_SCALAR_WIDE(F, P)				\
  PRED_##P (F, uint64_t, uint32_t, u32)				\
  PRED_##P (F, int64_t, int32_t, s32)

#define S_UNSIGNED(F, P)					\
  PRED_##P (F, svuint32_t, uint32_t, u32)

#define D_INTEGER(F, P)						\
  PRED_##P (F, svuint64_t, uint64_t, u64)			\
  PRED_##P (F, svint64_t, int64_t, s64)

#define D_INTEGER_SCALAR(F, P)					\
  PRED_##P (F, uint64_t, uint64_t, u64)				\
  PRED_##P (F, int64_t, int64_t, s64)

#define SD_INTEGER(F, P)					\
  S_INTEGER (F, P)						\
  D_INTEGER (F, P)

#define SD_DATA(F, P)						\
  PRED_##P (F, svfloat32_t, float32_t, f32)			\
  PRED_##P (F, svfloat64_t, float64_t, f64)			\
  S_INTEGER (F, P)						\
  D_INTEGER (F, P)

#define ALL_SIGNED(F, P)					\
  PRED_##P (F, svint8_t, int8_t, s8)				\
  PRED_##P (F, svint16_t, int16_t, s16)				\
  PRED_##P (F, svint32_t, int32_t, s32)				\
  PRED_##P (F, svint64_t, int64_t, s64)

#define ALL_SIGNED_UINT(F, P)					\
  PRED_##P (F, svuint8_t, int8_t, s8)				\
  PRED_##P (F, svuint16_t, int16_t, s16)			\
  PRED_##P (F, svuint32_t, int32_t, s32)			\
  PRED_##P (F, svuint64_t, int64_t, s64)

#define ALL_UNSIGNED_UINT(F, P)					\
  PRED_##P (F, svuint8_t, uint8_t, u8)				\
  PRED_##P (F, svuint16_t, uint16_t, u16)			\
  PRED_##P (F, svuint32_t, uint32_t, u32)			\
  PRED_##P (F, svuint64_t, uint64_t, u64)

#define HSD_INTEGER(F, P)					\
  H_INTEGER (F, P)						\
  S_INTEGER (F, P)						\
  D_INTEGER (F, P)

#define ALL_INTEGER(F, P)					\
  Q_INTEGER (F, P)						\
  HSD_INTEGER (F, P)

#define ALL_INTEGER_SCALAR(F, P)				\
  Q_INTEGER_SCALAR (F, P)					\
  H_INTEGER_SCALAR (F, P)					\
  S_INTEGER_SCALAR (F, P)					\
  D_INTEGER_SCALAR (F, P)

#define ALL_INTEGER_SCALAR_WIDE(F, P)				\
  Q_INTEGER_SCALAR_WIDE (F, P)					\
  H_INTEGER_SCALAR_WIDE (F, P)					\
  S_INTEGER_SCALAR_WIDE (F, P)					\
  D_INTEGER_SCALAR (F, P)

#define ALL_INTEGER_UINT(F, P)					\
  ALL_SIGNED_UINT (F, P)					\
  ALL_UNSIGNED_UINT (F, P)

#define ALL_FLOAT(F, P)						\
  PRED_##P (F, svfloat16_t, float16_t, f16)			\
  PRED_##P (F, svfloat32_t, float32_t, f32)			\
  PRED_##P (F, svfloat64_t, float64_t, f64)		

#define ALL_FLOAT_SCALAR(F, P)					\
  PRED_##P (F, float16_t, float16_t, f16)			\
  PRED_##P (F, float32_t, float32_t, f32)			\
  PRED_##P (F, float64_t, float64_t, f64)		

#define ALL_FLOAT_INT(F, P)					\
  PRED_##P (F, svint16_t, float16_t, f16)			\
  PRED_##P (F, svint32_t, float32_t, f32)			\
  PRED_##P (F, svint64_t, float64_t, f64)		

#define ALL_FLOAT_UINT(F, P)					\
  PRED_##P (F, svuint16_t, float16_t, f16)			\
  PRED_##P (F, svuint32_t, float32_t, f32)			\
  PRED_##P (F, svuint64_t, float64_t, f64)		

#define ALL_FLOAT_AND_SIGNED(F, P)				\
  ALL_SIGNED (F, P)						\
  ALL_FLOAT (F, P)

#define ALL_ARITH_SCALAR(F, P)					\
  ALL_INTEGER_SCALAR (F, P)					\
  ALL_FLOAT_SCALAR (F, P)

#define ALL_ARITH_SCALAR_WIDE(F, P)				\
  ALL_INTEGER_SCALAR_WIDE (F, P)				\
  ALL_FLOAT_SCALAR (F, P)

#define ALL_DATA(F, P)						\
  ALL_INTEGER (F, P)						\
  ALL_FLOAT (F, P)						\
  PRED_##P (F, svbfloat16_t, bfloat16_t, bf16)

#define ALL_DATA_SCALAR(F, P)					\
  ALL_ARITH_SCALAR (F, P)					\
  PRED_##P (F, bfloat16_t, bfloat16_t, bf16)

#define ALL_DATA_UINT(F, P)					\
  ALL_INTEGER_UINT (F, P)					\
  ALL_FLOAT_UINT (F, P)						\
  PRED_##P (F, svuint16_t, bfloat16_t, bf16)

#define B(F, P)							\
  PRED_##P (F, svbool_t, bool_t, b)

#define BN(F, P)						\
  PRED_##P (F, svbool_t, bool_t, b8)				\
  PRED_##P (F, svbool_t, bool_t, b16)				\
  PRED_##P (F, svbool_t, bool_t, b32)				\
  PRED_##P (F, svbool_t, bool_t, b64)

#define BOOL(F, P)						\
  PRED_##P (F, bool, bool_t)

#define ALL_PRED_UINT64(F, P)					\
  PRED_##P (F, uint64_t, bool_t, b8)				\
  PRED_##P (F, uint64_t, bool_t, b16)				\
  PRED_##P (F, uint64_t, bool_t, b32)				\
  PRED_##P (F, uint64_t, bool_t, b64)
