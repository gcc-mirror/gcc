/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC target "+rcpc3"

#include "arm_neon_test.h"

/*
** test_vstl1_lane_u64:
** stl1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1_lane_u64, uint64_t *, uint64x1_t)

/*
** test_vstl1_lane_s64:
** stl1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1_lane_s64, int64_t *, int64x1_t)

/*
** test_vstl1_lane_f64:
** stl1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1_lane_f64, float64_t *, float64x1_t)

/*
** test_vstl1_lane_p64:
** stl1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1_lane_p64, poly64_t *, poly64x1_t)

/*
** test_vstl1q_lane_u64:
** stl1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1q_lane_u64, uint64_t *, uint64x2_t)

/*
** test_vstl1q_lane_s64:
** stl1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1q_lane_s64, int64_t *, int64x2_t)

/*
** test_vstl1q_lane_f64:
** stl1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1q_lane_f64, float64_t *, float64x2_t)

/*
** test_vstl1q_lane_p64:
** stl1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_STORE_LANE (vstl1q_lane_p64, poly64_t *, poly64x2_t)
