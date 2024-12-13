/* { dg-do compile } */
/* { dg-additional-options "-O3 -march=armv9-a+lut" } */
/* { dg-final { check-function-bodies "**" ""} } */

#include "arm_neon.h"

/*
** test_vluti2_laneu8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2_laneu8(uint8x8_t a, uint8x8_t b, uint8x16_t results[])
{
  results[0] = vluti2_lane_u8(a, b, 0);
  results[1] = vluti2_lane_u8(a, b, 1);
}

/*
** test_vluti2_lanequ8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_lanequ8(uint8x8_t a, uint8x16_t b, uint8x16_t results[])
{
  results[0] = vluti2_laneq_u8(a, b, 0);
  results[1] = vluti2_laneq_u8(a, b, 1);
  results[2] = vluti2_laneq_u8(a, b, 2);
  results[3] = vluti2_laneq_u8(a, b, 3);
}

/*
** test_vluti2q_laneu8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2q_laneu8(uint8x16_t a, uint8x8_t b, uint8x16_t results[])
{
  results[0] = vluti2q_lane_u8(a, b, 0);
  results[1] = vluti2q_lane_u8(a, b, 1);
}

/*
** test_vluti2q_lanequ8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_lanequ8(uint8x16_t a, uint8x16_t b, uint8x16_t results[])
{
  results[0] = vluti2q_laneq_u8(a, b, 0);
  results[1] = vluti2q_laneq_u8(a, b, 1);
  results[2] = vluti2q_laneq_u8(a, b, 2);
  results[3] = vluti2q_laneq_u8(a, b, 3);
}

/*
** test_vluti2_lanes8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2_lanes8(int8x8_t a, uint8x8_t b, int8x16_t results[])
{
  results[0] = vluti2_lane_s8(a, b, 0);
  results[1] = vluti2_lane_s8(a, b, 1);
}

/*
** test_vluti2_laneqs8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_laneqs8(int8x8_t a, uint8x16_t b, int8x16_t results[])
{
  results[0] = vluti2_laneq_s8(a, b, 0);
  results[1] = vluti2_laneq_s8(a, b, 1);
  results[2] = vluti2_laneq_s8(a, b, 2);
  results[3] = vluti2_laneq_s8(a, b, 3);
}

/*
** test_vluti2q_lanes8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2q_lanes8(int8x16_t a, uint8x8_t b, int8x16_t results[])
{
  results[0] = vluti2q_lane_s8(a, b, 0);
  results[1] = vluti2q_lane_s8(a, b, 1);
}

/*
** test_vluti2q_laneqs8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_laneqs8(int8x16_t a, uint8x16_t b, int8x16_t results[])
{
  results[0] = vluti2q_laneq_s8(a, b, 0);
  results[1] = vluti2q_laneq_s8(a, b, 1);
  results[2] = vluti2q_laneq_s8(a, b, 2);
  results[3] = vluti2q_laneq_s8(a, b, 3);
}

/*
** test_vluti2_lanep8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2_lanep8(poly8x8_t a, uint8x8_t b, poly8x16_t results[])
{
  results[0] = vluti2_lane_p8(a, b, 0);
  results[1] = vluti2_lane_p8(a, b, 1);
}

/*
** test_vluti2_laneqp8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_laneqp8(poly8x8_t a, uint8x16_t b, poly8x16_t results[])
{
  results[0] = vluti2_laneq_p8(a, b, 0);
  results[1] = vluti2_laneq_p8(a, b, 1);
  results[2] = vluti2_laneq_p8(a, b, 2);
  results[3] = vluti2_laneq_p8(a, b, 3);
}

/*
** test_vluti2q_lanep8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti2q_lanep8(poly8x16_t a, uint8x8_t b, poly8x16_t results[])
{
  results[0] = vluti2q_lane_p8(a, b, 0);
  results[1] = vluti2q_lane_p8(a, b, 1);
}

/*
** test_vluti2q_laneqp8:
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_laneqp8(poly8x16_t a, uint8x16_t b, poly8x16_t results[])
{
  results[0] = vluti2q_laneq_p8(a, b, 0);
  results[1] = vluti2q_laneq_p8(a, b, 1);
  results[2] = vluti2q_laneq_p8(a, b, 2);
  results[3] = vluti2q_laneq_p8(a, b, 3);
}

/*
** test_vluti2_laneu16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_laneu16(uint16x4_t a, uint8x8_t b, uint16x8_t results[])
{
  results[0] = vluti2_lane_u16(a, b, 0);
  results[1] = vluti2_lane_u16(a, b, 1);
  results[2] = vluti2_lane_u16(a, b, 2);
  results[3] = vluti2_lane_u16(a, b, 3);
}

/*
** test_vluti2_lanequ16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2_lanequ16(uint16x4_t a, uint8x16_t b, uint16x8_t results[])
{
  results[0] = vluti2_laneq_u16(a, b, 0);
  results[1] = vluti2_laneq_u16(a, b, 1);
  results[2] = vluti2_laneq_u16(a, b, 2);
  results[3] = vluti2_laneq_u16(a, b, 3);
  results[4] = vluti2_laneq_u16(a, b, 4);
  results[5] = vluti2_laneq_u16(a, b, 5);
  results[6] = vluti2_laneq_u16(a, b, 6);
  results[7] = vluti2_laneq_u16(a, b, 7);
}

/*
** test_vluti2q_laneu16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_laneu16(uint16x8_t a, uint8x8_t b, uint16x8_t results[])
{
  results[0] = vluti2q_lane_u16(a, b, 0);
  results[1] = vluti2q_lane_u16(a, b, 1);
  results[2] = vluti2q_lane_u16(a, b, 2);
  results[3] = vluti2q_lane_u16(a, b, 3);
}

/*
** test_vluti2q_lanequ16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2q_lanequ16(uint16x8_t a, uint8x16_t b, uint16x8_t results[])
{
  results[0] = vluti2q_laneq_u16(a, b, 0);
  results[1] = vluti2q_laneq_u16(a, b, 1);
  results[2] = vluti2q_laneq_u16(a, b, 2);
  results[3] = vluti2q_laneq_u16(a, b, 3);
  results[4] = vluti2q_laneq_u16(a, b, 4);
  results[5] = vluti2q_laneq_u16(a, b, 5);
  results[6] = vluti2q_laneq_u16(a, b, 6);
  results[7] = vluti2q_laneq_u16(a, b, 7);
}

/*
** test_vluti2_lanes16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_lanes16(int16x4_t a, uint8x8_t b, int16x8_t results[])
{
  results[0] = vluti2_lane_s16(a, b, 0);
  results[1] = vluti2_lane_s16(a, b, 1);
  results[2] = vluti2_lane_s16(a, b, 2);
  results[3] = vluti2_lane_s16(a, b, 3);
}

/*
** test_vluti2_laneqs16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2_laneqs16(int16x4_t a, uint8x16_t b, int16x8_t results[])
{
  results[0] = vluti2_laneq_s16(a, b, 0);
  results[1] = vluti2_laneq_s16(a, b, 1);
  results[2] = vluti2_laneq_s16(a, b, 2);
  results[3] = vluti2_laneq_s16(a, b, 3);
  results[4] = vluti2_laneq_s16(a, b, 4);
  results[5] = vluti2_laneq_s16(a, b, 5);
  results[6] = vluti2_laneq_s16(a, b, 6);
  results[7] = vluti2_laneq_s16(a, b, 7);
}

/*
** test_vluti2q_lanes16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_lanes16(int16x8_t a, uint8x8_t b, int16x8_t results[])
{
  results[0] = vluti2q_lane_s16(a, b, 0);
  results[1] = vluti2q_lane_s16(a, b, 1);
  results[2] = vluti2q_lane_s16(a, b, 2);
  results[3] = vluti2q_lane_s16(a, b, 3);
}

/*
** test_vluti2q_laneqs16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2q_laneqs16(int16x8_t a, uint8x16_t b, int16x8_t results[])
{
  results[0] = vluti2q_laneq_s16(a, b, 0);
  results[1] = vluti2q_laneq_s16(a, b, 1);
  results[2] = vluti2q_laneq_s16(a, b, 2);
  results[3] = vluti2q_laneq_s16(a, b, 3);
  results[4] = vluti2q_laneq_s16(a, b, 4);
  results[5] = vluti2q_laneq_s16(a, b, 5);
  results[6] = vluti2q_laneq_s16(a, b, 6);
  results[7] = vluti2q_laneq_s16(a, b, 7);
}

/*
** test_vluti2_lanep16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_lanep16(poly16x4_t a, uint8x8_t b, poly16x8_t results[])
{
  results[0] = vluti2_lane_p16(a, b, 0);
  results[1] = vluti2_lane_p16(a, b, 1);
  results[2] = vluti2_lane_p16(a, b, 2);
  results[3] = vluti2_lane_p16(a, b, 3);
}

/*
** test_vluti2_laneqp16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2_laneqp16(poly16x4_t a, uint8x16_t b, poly16x8_t results[])
{
  results[0] = vluti2_laneq_p16(a, b, 0);
  results[1] = vluti2_laneq_p16(a, b, 1);
  results[2] = vluti2_laneq_p16(a, b, 2);
  results[3] = vluti2_laneq_p16(a, b, 3);
  results[4] = vluti2_laneq_p16(a, b, 4);
  results[5] = vluti2_laneq_p16(a, b, 5);
  results[6] = vluti2_laneq_p16(a, b, 6);
  results[7] = vluti2_laneq_p16(a, b, 7);
}

/*
** test_vluti2q_lanep16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_lanep16(poly16x8_t a, uint8x8_t b, poly16x8_t results[])
{
  results[0] = vluti2q_lane_p16(a, b, 0);
  results[1] = vluti2q_lane_p16(a, b, 1);
  results[2] = vluti2q_lane_p16(a, b, 2);
  results[3] = vluti2q_lane_p16(a, b, 3);
}

/*
** test_vluti2q_laneqp16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2q_laneqp16(poly16x8_t a, uint8x16_t b, poly16x8_t results[])
{
  results[0] = vluti2q_laneq_p16(a, b, 0);
  results[1] = vluti2q_laneq_p16(a, b, 1);
  results[2] = vluti2q_laneq_p16(a, b, 2);
  results[3] = vluti2q_laneq_p16(a, b, 3);
  results[4] = vluti2q_laneq_p16(a, b, 4);
  results[5] = vluti2q_laneq_p16(a, b, 5);
  results[6] = vluti2q_laneq_p16(a, b, 6);
  results[7] = vluti2q_laneq_p16(a, b, 7);
}

/*
** test_vluti2_lanef16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_lanef16(float16x4_t a, uint8x8_t b, float16x8_t results[])
{
  results[0] = vluti2_lane_f16(a, b, 0);
  results[1] = vluti2_lane_f16(a, b, 1);
  results[2] = vluti2_lane_f16(a, b, 2);
  results[3] = vluti2_lane_f16(a, b, 3);
}

/*
** test_vluti2_laneqf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2_laneqf16(float16x4_t a, uint8x16_t b, float16x8_t results[])
{
  results[0] = vluti2_laneq_f16(a, b, 0);
  results[1] = vluti2_laneq_f16(a, b, 1);
  results[2] = vluti2_laneq_f16(a, b, 2);
  results[3] = vluti2_laneq_f16(a, b, 3);
  results[4] = vluti2_laneq_f16(a, b, 4);
  results[5] = vluti2_laneq_f16(a, b, 5);
  results[6] = vluti2_laneq_f16(a, b, 6);
  results[7] = vluti2_laneq_f16(a, b, 7);
}

/*
** test_vluti2q_lanef16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_lanef16(float16x8_t a, uint8x8_t b, float16x8_t results[])
{
  results[0] = vluti2q_lane_f16(a, b, 0);
  results[1] = vluti2q_lane_f16(a, b, 1);
  results[2] = vluti2q_lane_f16(a, b, 2);
  results[3] = vluti2q_lane_f16(a, b, 3);
}

/*
** test_vluti2q_laneqf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2q_laneqf16(float16x8_t a, uint8x16_t b, float16x8_t results[])
{
  results[0] = vluti2q_laneq_f16(a, b, 0);
  results[1] = vluti2q_laneq_f16(a, b, 1);
  results[2] = vluti2q_laneq_f16(a, b, 2);
  results[3] = vluti2q_laneq_f16(a, b, 3);
  results[4] = vluti2q_laneq_f16(a, b, 4);
  results[5] = vluti2q_laneq_f16(a, b, 5);
  results[6] = vluti2q_laneq_f16(a, b, 6);
  results[7] = vluti2q_laneq_f16(a, b, 7);
}

/*
** test_vluti2_lanebf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2_lanebf16(bfloat16x4_t a, uint8x8_t b, bfloat16x8_t results[])
{
  results[0] = vluti2_lane_bf16(a, b, 0);
  results[1] = vluti2_lane_bf16(a, b, 1);
  results[2] = vluti2_lane_bf16(a, b, 2);
  results[3] = vluti2_lane_bf16(a, b, 3);
}

/*
** test_vluti2_laneqbf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2_laneqbf16(bfloat16x4_t a, uint8x16_t b, bfloat16x8_t results[])
{
  results[0] = vluti2_laneq_bf16(a, b, 0);
  results[1] = vluti2_laneq_bf16(a, b, 1);
  results[2] = vluti2_laneq_bf16(a, b, 2);
  results[3] = vluti2_laneq_bf16(a, b, 3);
  results[4] = vluti2_laneq_bf16(a, b, 4);
  results[5] = vluti2_laneq_bf16(a, b, 5);
  results[6] = vluti2_laneq_bf16(a, b, 6);
  results[7] = vluti2_laneq_bf16(a, b, 7);
}

/*
** test_vluti2q_lanebf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti2q_lanebf16(bfloat16x8_t a, uint8x8_t b, bfloat16x8_t results[])
{
  results[0] = vluti2q_lane_bf16(a, b, 0);
  results[1] = vluti2q_lane_bf16(a, b, 1);
  results[2] = vluti2q_lane_bf16(a, b, 2);
  results[3] = vluti2q_lane_bf16(a, b, 3);
}

/*
** test_vluti2q_laneqbf16:
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[3\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[4\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[5\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[6\]
**	luti2	v[0-9]+\.8h, {v[0-9]+\.8h}, v[0-9]+\[7\]
**	...
**	ret
*/
void
test_vluti2q_laneqbf16(bfloat16x8_t a, uint8x16_t b, bfloat16x8_t results[])
{
  results[0] = vluti2q_laneq_bf16(a, b, 0);
  results[1] = vluti2q_laneq_bf16(a, b, 1);
  results[2] = vluti2q_laneq_bf16(a, b, 2);
  results[3] = vluti2q_laneq_bf16(a, b, 3);
  results[4] = vluti2q_laneq_bf16(a, b, 4);
  results[5] = vluti2q_laneq_bf16(a, b, 5);
  results[6] = vluti2q_laneq_bf16(a, b, 6);
  results[7] = vluti2q_laneq_bf16(a, b, 7);
}

/*
** test_vluti4q_laneu8:
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	...
**	ret
*/
void
test_vluti4q_laneu8(uint8x16_t a, uint8x8_t b, uint8x16_t results[])
{
  results[0] = vluti4q_lane_u8(a, b, 0);
}

/*
** test_vluti4q_lanequ8:
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_lanequ8(uint8x16_t a, uint8x16_t b, uint8x16_t results[])
{
  results[0] = vluti4q_laneq_u8(a, b, 0);
  results[1] = vluti4q_laneq_u8(a, b, 1);
}

/*
** test_vluti4q_lanep8:
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	...
**	ret
*/
void
test_vluti4q_lanep8(poly8x16_t a, uint8x8_t b, poly8x16_t results[])
{
  results[0] = vluti4q_lane_p8(a, b, 0);
}

/*
** test_vluti4q_laneqp8:
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.16b, {v[0-9]+\.16b}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_laneqp8(poly8x16_t a, uint8x16_t b, poly8x16_t results[])
{
  results[0] = vluti4q_laneq_p8(a, b, 0);
  results[1] = vluti4q_laneq_p8(a, b, 1);
}

/*
** test_vluti4q_laneu16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_laneu16_x2(uint16x8x2_t a, uint8x8_t b, uint16x8_t results[])
{
  results[0] = vluti4q_lane_u16_x2(a, b, 0);
  results[1] = vluti4q_lane_u16_x2(a, b, 1);
}

/*
** test_vluti4q_lanequ16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti4q_lanequ16_x2(uint16x8x2_t a, uint8x16_t b, uint16x8_t results[])
{
  results[0] = vluti4q_laneq_u16_x2(a, b, 0);
  results[1] = vluti4q_laneq_u16_x2(a, b, 1);
  results[2] = vluti4q_laneq_u16_x2(a, b, 2);
  results[3] = vluti4q_laneq_u16_x2(a, b, 3);
}

/*
** test_vluti4q_lanes16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_lanes16_x2(int16x8x2_t a, uint8x8_t b, int16x8_t results[])
{
  results[0] = vluti4q_lane_s16_x2(a, b, 0);
  results[1] = vluti4q_lane_s16_x2(a, b, 1);
}

/*
** test_vluti4q_laneqs16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti4q_laneqs16_x2(int16x8x2_t a, uint8x16_t b, int16x8_t results[])
{
  results[0] = vluti4q_laneq_s16_x2(a, b, 0);
  results[1] = vluti4q_laneq_s16_x2(a, b, 1);
  results[2] = vluti4q_laneq_s16_x2(a, b, 2);
  results[3] = vluti4q_laneq_s16_x2(a, b, 3);
}

/*
** test_vluti4q_lanep16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_lanep16_x2(poly16x8x2_t a, uint8x8_t b, poly16x8_t results[])
{
  results[0] = vluti4q_lane_p16_x2(a, b, 0);
  results[1] = vluti4q_lane_p16_x2(a, b, 1);
}

/*
** test_vluti4q_laneqp16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti4q_laneqp16_x2(poly16x8x2_t a, uint8x16_t b, poly16x8_t results[])
{
  results[0] = vluti4q_laneq_p16_x2(a, b, 0);
  results[1] = vluti4q_laneq_p16_x2(a, b, 1);
  results[2] = vluti4q_laneq_p16_x2(a, b, 2);
  results[3] = vluti4q_laneq_p16_x2(a, b, 3);
}

/*
** test_vluti4q_lanef16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_lanef16_x2(float16x8x2_t a, uint8x8_t b, float16x8_t results[])
{
  results[0] = vluti4q_lane_f16_x2(a, b, 0);
  results[1] = vluti4q_lane_f16_x2(a, b, 1);
}

/*
** test_vluti4q_laneqf16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti4q_laneqf16_x2(float16x8x2_t a, uint8x16_t b, float16x8_t results[])
{
  results[0] = vluti4q_laneq_f16_x2(a, b, 0);
  results[1] = vluti4q_laneq_f16_x2(a, b, 1);
  results[2] = vluti4q_laneq_f16_x2(a, b, 2);
  results[3] = vluti4q_laneq_f16_x2(a, b, 3);
}

/*
** test_vluti4q_lanebf16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	...
**	ret
*/
void
test_vluti4q_lanebf16_x2(bfloat16x8x2_t a, uint8x8_t b, bfloat16x8_t results[])
{
  results[0] = vluti4q_lane_bf16_x2(a, b, 0);
  results[1] = vluti4q_lane_bf16_x2(a, b, 1);
}

/*
** test_vluti4q_laneqbf16_x2:
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[0\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[1\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[2\]
**	luti4	v[0-9]+\.8h, {v[0-9]+\.8h, v[0-9]+\.8h}, v[0-9]+\[3\]
**	...
**	ret
*/
void
test_vluti4q_laneqbf16_x2(bfloat16x8x2_t a, uint8x16_t b, bfloat16x8_t results[])
{
  results[0] = vluti4q_laneq_bf16_x2(a, b, 0);
  results[1] = vluti4q_laneq_bf16_x2(a, b, 1);
  results[2] = vluti4q_laneq_bf16_x2(a, b, 2);
  results[3] = vluti4q_laneq_bf16_x2(a, b, 3);
}
