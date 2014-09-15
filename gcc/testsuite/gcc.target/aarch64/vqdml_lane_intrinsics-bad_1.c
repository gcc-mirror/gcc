/* { dg-do compile } */

#include "arm_neon.h"

int32x4_t
foo (int32x4_t a, int16x4_t b, int16x4_t c, int d)
{
  return vqdmlal_lane_s16 (a, b, c, d);
}

int32x4_t
foo1 (int32x4_t a, int16x4_t b, int16x8_t c, int d)
{
  return vqdmlal_laneq_s16 (a, b, c, d);
}

int32x4_t
foo2 (int32x4_t a, int16x4_t b, int16x4_t c, int d)
{
  return vqdmlsl_lane_s16 (a, b, c, d);
}

int32x4_t
foo3 (int32x4_t a, int16x4_t b, int16x8_t c, int d)
{
  return vqdmlsl_laneq_s16 (a, b, c, d);
}

int32x4_t
foo4 (int32x4_t a, int16x8_t b, int16x4_t c, int d)
{
  return vqdmlal_high_lane_s16 (a, b, c, d);
}

int32x4_t
foo5 (int32x4_t a, int16x8_t b, int16x4_t c, int d)
{
  return vqdmlsl_high_lane_s16 (a, b, c, d);
}

int32x4_t
foo6 (int32x4_t a, int16x8_t b, int16x8_t c, int d)
{
  return vqdmlal_high_laneq_s16 (a, b, c, d);
}

int32x4_t
foo7 (int32x4_t a, int16x8_t b, int16x8_t c, int d)
{
  return vqdmlsl_high_laneq_s16 (a, b, c, d);
}


/* { dg-excess-errors "incompatible type for argument" } */
