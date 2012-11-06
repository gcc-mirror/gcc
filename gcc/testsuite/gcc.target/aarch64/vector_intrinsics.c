/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "../../../config/aarch64/arm_neon.h"


/* { dg-final { scan-assembler-times "\\tfmax\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

float32x2_t
test_vmax_f32 (float32x2_t __a, float32x2_t __b)
{
  return vmax_f32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.8b, v\[0-9\].8b, v\[0-9\].8b" 1 } } */

int8x8_t
test_vmax_s8 (int8x8_t __a, int8x8_t __b)
{
  return vmax_s8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.8b, v\[0-9\].8b, v\[0-9\].8b" 1 } } */

uint8x8_t
test_vmax_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return vmax_u8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.4h, v\[0-9\].4h, v\[0-9\].4h" 1 } } */

int16x4_t
test_vmax_s16 (int16x4_t __a, int16x4_t __b)
{
  return vmax_s16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.4h, v\[0-9\].4h, v\[0-9\].4h" 1 } } */

uint16x4_t
test_vmax_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return vmax_u16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

int32x2_t
test_vmax_s32 (int32x2_t __a, int32x2_t __b)
{
  return vmax_s32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

uint32x2_t
test_vmax_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return vmax_u32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tfmax\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

float32x4_t
test_vmaxq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vmaxq_f32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tfmax\\tv\[0-9\]+\.2d, v\[0-9\].2d, v\[0-9\].2d" 1 } } */

float64x2_t
test_vmaxq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vmaxq_f64(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.16b, v\[0-9\].16b, v\[0-9\].16b" 1 } } */

int8x16_t
test_vmaxq_s8 (int8x16_t __a, int8x16_t __b)
{
  return vmaxq_s8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.16b, v\[0-9\].16b, v\[0-9\].16b" 1 } } */

uint8x16_t
test_vmaxq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return vmaxq_u8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.8h, v\[0-9\].8h, v\[0-9\].8h" 1 } } */

int16x8_t
test_vmaxq_s16 (int16x8_t __a, int16x8_t __b)
{
  return vmaxq_s16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.8h, v\[0-9\].8h, v\[0-9\].8h" 1 } } */

uint16x8_t
test_vmaxq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return vmaxq_u16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmax\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

int32x4_t
test_vmaxq_s32 (int32x4_t __a, int32x4_t __b)
{
  return vmaxq_s32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumax\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

uint32x4_t
test_vmaxq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return vmaxq_u32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tfmin\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

float32x2_t
test_vmin_f32 (float32x2_t __a, float32x2_t __b)
{
  return vmin_f32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.8b, v\[0-9\].8b, v\[0-9\].8b" 1 } } */

int8x8_t
test_vmin_s8 (int8x8_t __a, int8x8_t __b)
{
  return vmin_s8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.8b, v\[0-9\].8b, v\[0-9\].8b" 1 } } */

uint8x8_t
test_vmin_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return vmin_u8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.4h, v\[0-9\].4h, v\[0-9\].4h" 1 } } */

int16x4_t
test_vmin_s16 (int16x4_t __a, int16x4_t __b)
{
  return vmin_s16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.4h, v\[0-9\].4h, v\[0-9\].4h" 1 } } */

uint16x4_t
test_vmin_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return vmin_u16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

int32x2_t
test_vmin_s32 (int32x2_t __a, int32x2_t __b)
{
  return vmin_s32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 1 } } */

uint32x2_t
test_vmin_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return vmin_u32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tfmin\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

float32x4_t
test_vminq_f32 (float32x4_t __a, float32x4_t __b)
{
  return vminq_f32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tfmin\\tv\[0-9\]+\.2d, v\[0-9\].2d, v\[0-9\].2d" 1 } } */

float64x2_t
test_vminq_f64 (float64x2_t __a, float64x2_t __b)
{
  return vminq_f64(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.16b, v\[0-9\].16b, v\[0-9\].16b" 1 } } */

int8x16_t
test_vminq_s8 (int8x16_t __a, int8x16_t __b)
{
  return vminq_s8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.16b, v\[0-9\].16b, v\[0-9\].16b" 1 } } */

uint8x16_t
test_vminq_u8 (uint8x16_t __a, uint8x16_t __b)
{
  return vminq_u8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.8h, v\[0-9\].8h, v\[0-9\].8h" 1 } } */

int16x8_t
test_vminq_s16 (int16x8_t __a, int16x8_t __b)
{
  return vminq_s16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.8h, v\[0-9\].8h, v\[0-9\].8h" 1 } } */

uint16x8_t
test_vminq_u16 (uint16x8_t __a, uint16x8_t __b)
{
  return vminq_u16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsmin\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

int32x4_t
test_vminq_s32 (int32x4_t __a, int32x4_t __b)
{
  return vminq_s32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tumin\\tv\[0-9\]+\.4s, v\[0-9\].4s, v\[0-9\].4s" 1 } } */

uint32x4_t
test_vminq_u32 (uint32x4_t __a, uint32x4_t __b)
{
  return vminq_u32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\taddp\\tv\[0-9\]+\.8b, v\[0-9\].8b, v\[0-9\].8b" 2 } } */

int8x8_t
test_vpadd_s8 (int8x8_t __a, int8x8_t __b)
{
  return vpadd_s8(__a, __b);
}

uint8x8_t
test_vpadd_u8 (uint8x8_t __a, uint8x8_t __b)
{
  return vpadd_u8(__a, __b);
}

/* { dg-final { scan-assembler-times "\\taddp\\tv\[0-9\]+\.4h, v\[0-9\].4h, v\[0-9\].4h" 2 } } */

int16x4_t
test_vpadd_s16 (int16x4_t __a, int16x4_t __b)
{
  return vpadd_s16(__a, __b);
}

uint16x4_t
test_vpadd_u16 (uint16x4_t __a, uint16x4_t __b)
{
  return vpadd_u16(__a, __b);
}

/* { dg-final { scan-assembler-times "\\taddp\\tv\[0-9\]+\.2s, v\[0-9\].2s, v\[0-9\].2s" 2 } } */

int32x2_t
test_vpadd_s32 (int32x2_t __a, int32x2_t __b)
{
  return vpadd_s32(__a, __b);
}

uint32x2_t
test_vpadd_u32 (uint32x2_t __a, uint32x2_t __b)
{
  return vpadd_u32(__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */

int32x4_t
test_vqdmlal_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return vqdmlal_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */

int32x4_t
test_vqdmlal_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return vqdmlal_high_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmlal_high_lane_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  return vqdmlal_high_lane_s16 (a, b, c, 3);
}

int32x4_t
test_vqdmlal_high_laneq_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  return vqdmlal_high_laneq_s16 (a, b, c, 6);
}

int32x4_t
test_vqdmlal_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return vqdmlal_high_n_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmlal_lane_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  return vqdmlal_lane_s16 (a, b, c, 3);
}

int32x4_t
test_vqdmlal_laneq_s16 (int32x4_t a, int16x4_t b, int16x8_t c)
{
  return vqdmlal_laneq_s16 (a, b, c, 6);
}

int32x4_t
test_vqdmlal_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return vqdmlal_n_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

int64x2_t
test_vqdmlal_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return vqdmlal_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

int64x2_t
test_vqdmlal_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlal_high_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmlal_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlal_high_lane_s32 (__a, __b, __c, 1);
}

int64x2_t
test_vqdmlal_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlal_high_laneq_s32 (__a, __b, __c, 3);
}

int64x2_t
test_vqdmlal_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return vqdmlal_high_n_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlal\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmlal_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return vqdmlal_lane_s32 (__a, __b, __c, 1);
}

int64x2_t
test_vqdmlal_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c)
{
  return vqdmlal_laneq_s32 (__a, __b, __c, 3);
}

int64x2_t
test_vqdmlal_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return vqdmlal_n_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */

int32x4_t
test_vqdmlsl_s16 (int32x4_t __a, int16x4_t __b, int16x4_t __c)
{
  return vqdmlsl_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */

int32x4_t
test_vqdmlsl_high_s16 (int32x4_t __a, int16x8_t __b, int16x8_t __c)
{
  return vqdmlsl_high_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmlsl_high_lane_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  return vqdmlsl_high_lane_s16 (a, b, c, 3);
}

int32x4_t
test_vqdmlsl_high_laneq_s16 (int32x4_t a, int16x8_t b, int16x8_t c)
{
  return vqdmlsl_high_laneq_s16 (a, b, c, 6);
}

int32x4_t
test_vqdmlsl_high_n_s16 (int32x4_t __a, int16x8_t __b, int16_t __c)
{
  return vqdmlsl_high_n_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmlsl_lane_s16 (int32x4_t a, int16x4_t b, int16x4_t c)
{
  return vqdmlsl_lane_s16 (a, b, c, 3);
}

int32x4_t
test_vqdmlsl_laneq_s16 (int32x4_t a, int16x4_t b, int16x8_t c)
{
  return vqdmlsl_laneq_s16 (a, b, c, 6);
}

int32x4_t
test_vqdmlsl_n_s16 (int32x4_t __a, int16x4_t __b, int16_t __c)
{
  return vqdmlsl_n_s16 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

int64x2_t
test_vqdmlsl_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return vqdmlsl_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

int64x2_t
test_vqdmlsl_high_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlsl_high_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmlsl_high_lane_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlsl_high_lane_s32 (__a, __b, __c, 1);
}

int64x2_t
test_vqdmlsl_high_laneq_s32 (int64x2_t __a, int32x4_t __b, int32x4_t __c)
{
  return vqdmlsl_high_laneq_s32 (__a, __b, __c, 3);
}

int64x2_t
test_vqdmlsl_high_n_s32 (int64x2_t __a, int32x4_t __b, int32_t __c)
{
  return vqdmlsl_high_n_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmlsl\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmlsl_lane_s32 (int64x2_t __a, int32x2_t __b, int32x2_t __c)
{
  return vqdmlsl_lane_s32 (__a, __b, __c, 1);
}

int64x2_t
test_vqdmlsl_laneq_s32 (int64x2_t __a, int32x2_t __b, int32x4_t __c)
{
  return vqdmlsl_laneq_s32 (__a, __b, __c, 3);
}

int64x2_t
test_vqdmlsl_n_s32 (int64x2_t __a, int32x2_t __b, int32_t __c)
{
  return vqdmlsl_n_s32 (__a, __b, __c);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */

int32x4_t
test_vqdmull_s16 (int16x4_t __a, int16x4_t __b)
{
  return vqdmull_s16 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */

int32x4_t
test_vqdmull_high_s16 (int16x8_t __a, int16x8_t __b)
{
  return vqdmull_high_s16 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull2\\tv\[0-9\]+\.4s, v\[0-9\]+\.8h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmull_high_lane_s16 (int16x8_t a, int16x8_t b)
{
  return vqdmull_high_lane_s16 (a, b, 3);
}

int32x4_t
test_vqdmull_high_laneq_s16 (int16x8_t a, int16x8_t b)
{
  return vqdmull_high_laneq_s16 (a, b, 6);
}

int32x4_t
test_vqdmull_high_n_s16 (int16x8_t __a, int16_t __b)
{
  return vqdmull_high_n_s16 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\tv\[0-9\]+\.4s, v\[0-9\]+\.4h, v\[0-9\]+\.h" 3 } } */

int32x4_t
test_vqdmull_lane_s16 (int16x4_t a, int16x4_t b)
{
  return vqdmull_lane_s16 (a, b, 3);
}

int32x4_t
test_vqdmull_laneq_s16 (int16x4_t a, int16x8_t b)
{
  return vqdmull_laneq_s16 (a, b, 6);
}

int32x4_t
test_vqdmull_n_s16 (int16x4_t __a, int16_t __b)
{
  return vqdmull_n_s16 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

int64x2_t
test_vqdmull_s32 (int32x2_t __a, int32x2_t __b)
{
  return vqdmull_s32 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

int64x2_t
test_vqdmull_high_s32 (int32x4_t __a, int32x4_t __b)
{
  return vqdmull_high_s32 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull2\\tv\[0-9\]+\.2d, v\[0-9\]+\.4s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmull_high_lane_s32 (int32x4_t __a, int32x4_t __b)
{
  return vqdmull_high_lane_s32 (__a, __b, 1);
}

int64x2_t
test_vqdmull_high_laneq_s32 (int32x4_t __a, int32x4_t __b)
{
  return vqdmull_high_laneq_s32 (__a, __b, 3);
}

int64x2_t
test_vqdmull_high_n_s32 (int32x4_t __a, int32_t __b)
{
  return vqdmull_high_n_s32 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsqdmull\\tv\[0-9\]+\.2d, v\[0-9\]+\.2s, v\[0-9\]+\.s" 3 } } */

int64x2_t
test_vqdmull_lane_s32 (int32x2_t __a, int32x2_t __b)
{
  return vqdmull_lane_s32 (__a, __b, 1);
}

int64x2_t
test_vqdmull_laneq_s32 (int32x2_t __a, int32x4_t __b)
{
  return vqdmull_laneq_s32 (__a, __b, 1);
}

int64x2_t
test_vqdmull_n_s32 (int32x2_t __a, int32_t __b)
{
  return vqdmull_n_s32 (__a, __b);
}

/* { dg-final { scan-assembler-times "\\tsshll\\tv\[0-9\]+\.2d" 1 } } */

int64x2_t
test_vshll_n_s32 (int32x2_t __a)
{
  return vshll_n_s32 (__a, 9);
}

/* { dg-final { scan-assembler-times "\\tushll\\tv\[0-9\]+\.2d" 1 } } */

uint64x2_t
test_vshll_n_u32 (uint32x2_t __a)
{
  return vshll_n_u32 (__a, 9);
}

/* { dg-final { scan-assembler-times "\\tshll\\tv\[0-9\]+\.2d" 2 } } */

int64x2_t
test_vshll_n_s32_2 (int32x2_t __a)
{
  return vshll_n_s32 (__a, 32);
}

uint64x2_t
test_vshll_n_u32_2 (uint32x2_t __a)
{
  return vshll_n_u32 (__a, 32);
}

/* { dg-final { scan-assembler-times "\\tsshll\\tv\[0-9\]+\.4s" 1 } } */

int32x4_t
test_vshll_n_s16 (int16x4_t __a)
{
  return vshll_n_s16 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tushll\\tv\[0-9\]+\.4s" 1 } } */

uint32x4_t
test_vshll_n_u16 (uint16x4_t __a)
{
  return vshll_n_u16 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tshll\\tv\[0-9\]+\.4s" 2 } } */

int32x4_t
test_vshll_n_s16_2 (int16x4_t __a)
{
  return vshll_n_s16 (__a, 16);
}

uint32x4_t
test_vshll_n_u16_2 (uint16x4_t __a)
{
  return vshll_n_u16 (__a, 16);
}

/* { dg-final { scan-assembler-times "\\tsshll\\tv\[0-9\]+\.8h" 1 } } */

int16x8_t
test_vshll_n_s8 (int8x8_t __a)
{
  return vshll_n_s8 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tushll\\tv\[0-9\]+\.8h" 1 } } */

uint16x8_t
test_vshll_n_u8 (uint8x8_t __a)
{
  return vshll_n_u8 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tshll\\tv\[0-9\]+\.8h" 2 } } */

int16x8_t
test_vshll_n_s8_2 (int8x8_t __a)
{
  return vshll_n_s8 (__a, 8);
}

uint16x8_t
test_vshll_n_u8_2 (uint8x8_t __a)
{
  return vshll_n_u8 (__a, 8);
}

/* { dg-final { scan-assembler-times "\\tsshll2\\tv\[0-9\]+\.2d" 1 } } */

int64x2_t
test_vshll_high_n_s32 (int32x4_t __a)
{
  return vshll_high_n_s32 (__a, 9);
}

/* { dg-final { scan-assembler-times "\\tushll2\\tv\[0-9\]+\.2d" 1 } } */

uint64x2_t
test_vshll_high_n_u32 (uint32x4_t __a)
{
  return vshll_high_n_u32 (__a, 9);
}

/* { dg-final { scan-assembler-times "\\tshll2\\tv\[0-9\]+\.2d" 2 } } */

int64x2_t
test_vshll_high_n_s32_2 (int32x4_t __a)
{
  return vshll_high_n_s32 (__a, 32);
}

uint64x2_t
test_vshll_high_n_u32_2 (uint32x4_t __a)
{
  return vshll_high_n_u32 (__a, 32);
}

/* { dg-final { scan-assembler-times "\\tsshll2\\tv\[0-9\]+\.4s" 1 } } */

int32x4_t
test_vshll_high_n_s16 (int16x8_t __a)
{
  return vshll_high_n_s16 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tushll2\\tv\[0-9\]+\.4s" 1 } } */

uint32x4_t
test_vshll_high_n_u16 (uint16x8_t __a)
{
  return vshll_high_n_u16 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tshll2\\tv\[0-9\]+\.4s" 2 } } */

int32x4_t
test_vshll_high_n_s16_2 (int16x8_t __a)
{
  return vshll_high_n_s16 (__a, 16);
}

uint32x4_t
test_vshll_high_n_u16_2 (uint16x8_t __a)
{
  return vshll_high_n_u16 (__a, 16);
}

/* { dg-final { scan-assembler-times "\\tsshll2\\tv\[0-9\]+\.8h" 1 } } */

int16x8_t
test_vshll_high_n_s8 (int8x16_t __a)
{
  return vshll_high_n_s8 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tushll2\\tv\[0-9\]+\.8h" 1 } } */

uint16x8_t
test_vshll_high_n_u8 (uint8x16_t __a)
{
  return vshll_high_n_u8 (__a, 3);
}

/* { dg-final { scan-assembler-times "\\tshll2\\tv\[0-9\]+\.8h" 2 } } */

int16x8_t
test_vshll_high_n_s8_2 (int8x16_t __a)
{
  return vshll_high_n_s8 (__a, 8);
}

uint16x8_t
test_vshll_high_n_u8_2 (uint8x16_t __a)
{
  return vshll_high_n_u8 (__a, 8);
}
