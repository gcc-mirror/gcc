#include <arm_sve.h>

#define MXZ(F, RTY, TY1, TY2)					\
  RTY F##_f (TY1 op1, TY2 op2)					\
  {								\
    return sv##F (svpfalse_b (), op1, op2);			\
  }

#define PRED_MXv(F, RTY, TYPE1, TYPE2, TY)			\
  MXZ (F##_##TY##_m, RTY, TYPE1, sv##TYPE2)		\
  MXZ (F##_##TY##_x, RTY, TYPE1, sv##TYPE2)

#define PRED_Zv(F, RTY, TYPE1, TYPE2, TY)			\
  MXZ (F##_##TY##_z, RTY, TYPE1, sv##TYPE2)

#define PRED_MXZv(F, RTY, TYPE1, TYPE2, TY)			\
  PRED_MXv (F, RTY, TYPE1, TYPE2, TY)				\
  PRED_Zv (F, RTY, TYPE1, TYPE2, TY)

#define PRED_Z(F, RTY, TYPE1, TYPE2, TY)			\
  PRED_Zv (F, RTY, TYPE1, TYPE2, TY)				\
  MXZ (F##_n_##TY##_z, RTY, TYPE1, TYPE2)

#define PRED_MXZ(F, RTY, TYPE1, TYPE2, TY)			\
  PRED_MXv (F, RTY, TYPE1, TYPE2, TY)				\
  MXZ (F##_n_##TY##_m, RTY, TYPE1, TYPE2)		\
  MXZ (F##_n_##TY##_x, RTY, TYPE1, TYPE2)		\
  PRED_Z (F, RTY, TYPE1, TYPE2, TY)

#define PRED_IMPLICITv(F, RTY, TYPE1, TYPE2, TY)		\
  MXZ (F##_##TY, RTY, TYPE1, sv##TYPE2)

#define PRED_IMPLICIT(F, RTY, TYPE1, TYPE2, TY)			\
  PRED_IMPLICITv (F, RTY, TYPE1, TYPE2, TY)			\
  MXZ (F##_n_##TY, RTY, TYPE1, TYPE2)

#define ALL_Q_INTEGER(F, P)					\
  PRED_##P (F, svuint8_t, svuint8_t, uint8_t, u8)		\
  PRED_##P (F, svint8_t, svint8_t, int8_t, s8)

#define ALL_Q_INTEGER_UINT(F, P)				\
  PRED_##P (F, svuint8_t, svuint8_t, uint8_t, u8)		\
  PRED_##P (F, svint8_t, svint8_t, uint8_t, s8)

#define ALL_Q_INTEGER_INT(F, P)					\
  PRED_##P (F, svuint8_t, svuint8_t, int8_t, u8)		\
  PRED_##P (F, svint8_t, svint8_t, int8_t, s8)

#define ALL_Q_INTEGER_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svuint8_t, uint8_t, u8)		\
  PRED_##P (F, svbool_t, svint8_t, int8_t, s8)

#define ALL_H_INTEGER(F, P)					\
  PRED_##P (F, svuint16_t, svuint16_t, uint16_t, u16)		\
  PRED_##P (F, svint16_t, svint16_t, int16_t, s16)

#define ALL_H_INTEGER_UINT(F, P)				\
  PRED_##P (F, svuint16_t, svuint16_t, uint16_t, u16)		\
  PRED_##P (F, svint16_t, svint16_t, uint16_t, s16)

#define ALL_H_INTEGER_INT(F, P)					\
  PRED_##P (F, svuint16_t, svuint16_t, int16_t, u16)		\
  PRED_##P (F, svint16_t, svint16_t, int16_t, s16)

#define ALL_H_INTEGER_WIDE(F, P)				\
  PRED_##P (F, svuint16_t, svuint16_t, uint8_t, u16)		\
  PRED_##P (F, svint16_t, svint16_t, int8_t, s16)

#define ALL_H_INTEGER_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svuint16_t, uint16_t, u16)		\
  PRED_##P (F, svbool_t, svint16_t, int16_t, s16)

#define ALL_S_INTEGER(F, P)					\
  PRED_##P (F, svuint32_t, svuint32_t, uint32_t, u32)		\
  PRED_##P (F, svint32_t, svint32_t, int32_t, s32)

#define ALL_S_INTEGER_UINT(F, P)				\
  PRED_##P (F, svuint32_t, svuint32_t, uint32_t, u32)		\
  PRED_##P (F, svint32_t, svint32_t, uint32_t, s32)

#define ALL_S_INTEGER_INT(F, P)					\
  PRED_##P (F, svuint32_t, svuint32_t, int32_t, u32)		\
  PRED_##P (F, svint32_t, svint32_t, int32_t, s32)

#define ALL_S_INTEGER_WIDE(F, P)				\
  PRED_##P (F, svuint32_t, svuint32_t, uint16_t, u32)		\
  PRED_##P (F, svint32_t, svint32_t, int16_t, s32)

#define ALL_S_INTEGER_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svuint32_t, uint32_t, u32)		\
  PRED_##P (F, svbool_t, svint32_t, int32_t, s32)

#define ALL_D_INTEGER(F, P)					\
  PRED_##P (F, svuint64_t, svuint64_t, uint64_t, u64)		\
  PRED_##P (F, svint64_t, svint64_t, int64_t, s64)

#define ALL_D_INTEGER_UINT(F, P)				\
  PRED_##P (F, svuint64_t, svuint64_t, uint64_t, u64)		\
  PRED_##P (F, svint64_t, svint64_t, uint64_t, s64)

#define ALL_D_INTEGER_INT(F, P)					\
  PRED_##P (F, svuint64_t, svuint64_t, int64_t, u64)		\
  PRED_##P (F, svint64_t, svint64_t, int64_t, s64)

#define ALL_D_INTEGER_WIDE(F, P)				\
  PRED_##P (F, svuint64_t, svuint64_t, uint32_t, u64)		\
  PRED_##P (F, svint64_t, svint64_t, int32_t, s64)

#define ALL_D_INTEGER_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svuint64_t, uint64_t, u64)		\
  PRED_##P (F, svbool_t, svint64_t, int64_t, s64)

#define SD_INTEGER_TO_UINT(F, P)				\
  PRED_##P (F, svuint32_t, svuint32_t, uint32_t, u32)		\
  PRED_##P (F, svuint64_t, svuint64_t, uint64_t, u64)		\
  PRED_##P (F, svuint32_t, svint32_t, int32_t, s32)		\
  PRED_##P (F, svuint64_t, svint64_t, int64_t, s64)

#define BH_INTEGER_BOOL(F, P)					\
  ALL_Q_INTEGER_BOOL (F, P)					\
  ALL_H_INTEGER_BOOL (F, P)

#define BHS_UNSIGNED_UINT64(F, P)				\
  PRED_##P (F, svuint8_t, svuint8_t, uint64_t, u8)		\
  PRED_##P (F, svuint16_t, svuint16_t, uint64_t, u16)		\
  PRED_##P (F, svuint32_t, svuint32_t, uint64_t, u32)

#define BHS_UNSIGNED_WIDE_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svuint8_t, uint64_t, u8)		\
  PRED_##P (F, svbool_t, svuint16_t, uint64_t, u16)		\
  PRED_##P (F, svbool_t, svuint32_t, uint64_t, u32)

#define BHS_SIGNED_UINT64(F, P)					\
  PRED_##P (F, svint8_t, svint8_t, uint64_t, s8)		\
  PRED_##P (F, svint16_t, svint16_t, uint64_t, s16)		\
  PRED_##P (F, svint32_t, svint32_t, uint64_t, s32)

#define BHS_SIGNED_WIDE_BOOL(F, P)				\
  PRED_##P (F, svbool_t, svint8_t, int64_t, s8)		\
  PRED_##P (F, svbool_t, svint16_t, int64_t, s16)		\
  PRED_##P (F, svbool_t, svint32_t, int64_t, s32)

#define ALL_UNSIGNED_UINT(F, P)					\
  PRED_##P (F, svuint8_t, svuint8_t, uint8_t, u8)		\
  PRED_##P (F, svuint16_t, svuint16_t, uint16_t, u16)		\
  PRED_##P (F, svuint32_t, svuint32_t, uint32_t, u32)		\
  PRED_##P (F, svuint64_t, svuint64_t, uint64_t, u64)

#define ALL_UNSIGNED_INT(F, P)					\
  PRED_##P (F, svuint8_t, svuint8_t, int8_t, u8)		\
  PRED_##P (F, svuint16_t, svuint16_t, int16_t, u16)		\
  PRED_##P (F, svuint32_t, svuint32_t, int32_t, u32)		\
  PRED_##P (F, svuint64_t, svuint64_t, int64_t, u64)

#define ALL_SIGNED_UINT(F, P)					\
  PRED_##P (F, svint8_t, svint8_t, uint8_t, s8)		\
  PRED_##P (F, svint16_t, svint16_t, uint16_t, s16)		\
  PRED_##P (F, svint32_t, svint32_t, uint32_t, s32)		\
  PRED_##P (F, svint64_t, svint64_t, uint64_t, s64)

#define ALL_FLOAT(F, P)						\
  PRED_##P (F, svfloat16_t, svfloat16_t, float16_t, f16)	\
  PRED_##P (F, svfloat32_t, svfloat32_t, float32_t, f32)	\
  PRED_##P (F, svfloat64_t, svfloat64_t, float64_t, f64)

#define ALL_FLOAT_INT(F, P)					\
  PRED_##P (F, svfloat16_t, svfloat16_t, int16_t, f16)	\
  PRED_##P (F, svfloat32_t, svfloat32_t, int32_t, f32)	\
  PRED_##P (F, svfloat64_t, svfloat64_t, int64_t, f64)

#define ALL_FLOAT_BOOL(F, P)					\
  PRED_##P (F, svbool_t, svfloat16_t, float16_t, f16)		\
  PRED_##P (F, svbool_t, svfloat32_t, float32_t, f32)		\
  PRED_##P (F, svbool_t, svfloat64_t, float64_t, f64)

#define ALL_FLOAT_SCALAR(F, P)					\
  PRED_##P (F, float16_t, float16_t, float16_t, f16)	\
  PRED_##P (F, float32_t, float32_t, float32_t, f32)	\
  PRED_##P (F, float64_t, float64_t, float64_t, f64)

#define B(F, P)							\
  PRED_##P (F, svbool_t, svbool_t, bool_t, b)

#define ALL_SD_INTEGER(F, P)					\
  ALL_S_INTEGER (F, P)						\
  ALL_D_INTEGER (F, P)

#define HSD_INTEGER_WIDE(F, P)					\
  ALL_H_INTEGER_WIDE (F, P)					\
  ALL_S_INTEGER_WIDE (F, P)					\
  ALL_D_INTEGER_WIDE (F, P)

#define BHS_INTEGER_UINT64(F, P)				\
  BHS_UNSIGNED_UINT64 (F, P)					\
  BHS_SIGNED_UINT64 (F, P)

#define BHS_INTEGER_WIDE_BOOL(F, P)				\
  BHS_UNSIGNED_WIDE_BOOL (F, P)					\
  BHS_SIGNED_WIDE_BOOL (F, P)

#define ALL_INTEGER(F, P)					\
  ALL_Q_INTEGER (F, P)						\
  ALL_H_INTEGER (F, P)						\
  ALL_S_INTEGER (F, P)						\
  ALL_D_INTEGER (F, P)

#define ALL_INTEGER_UINT(F, P)					\
  ALL_Q_INTEGER_UINT (F, P)					\
  ALL_H_INTEGER_UINT (F, P)					\
  ALL_S_INTEGER_UINT (F, P)					\
  ALL_D_INTEGER_UINT (F, P)

#define ALL_INTEGER_INT(F, P)					\
  ALL_Q_INTEGER_INT (F, P)					\
  ALL_H_INTEGER_INT (F, P)					\
  ALL_S_INTEGER_INT (F, P)					\
  ALL_D_INTEGER_INT (F, P)

#define ALL_INTEGER_BOOL(F, P)					\
  ALL_Q_INTEGER_BOOL (F, P)					\
  ALL_H_INTEGER_BOOL (F, P)					\
  ALL_S_INTEGER_BOOL (F, P)					\
  ALL_D_INTEGER_BOOL (F, P)

#define ALL_FLOAT_AND_SD_INTEGER(F, P)				\
  ALL_SD_INTEGER (F, P)						\
  ALL_FLOAT (F, P)

#define ALL_ARITH(F, P)						\
  ALL_INTEGER (F, P)						\
  ALL_FLOAT (F, P)

#define ALL_ARITH_BOOL(F, P)					\
  ALL_INTEGER_BOOL (F, P)					\
  ALL_FLOAT_BOOL (F, P)

#define ALL_DATA(F, P)						\
  ALL_ARITH (F, P)						\
  PRED_##P (F, svbfloat16_t, svbfloat16_t, bfloat16_t, bf16)

