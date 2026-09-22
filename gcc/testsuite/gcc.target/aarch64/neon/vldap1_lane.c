/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC target "+rcpc3"

#include "arm_neon_test.h"

/*
** test_vldap1_lane_u64:
** ldap1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1_lane_u64, const uint64_t *, uint64x1_t)

/*
** test_vldap1_lane_s64:
** ldap1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1_lane_s64, const int64_t *, int64x1_t)

/*
** test_vldap1_lane_f64:
** ldap1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1_lane_f64, const float64_t *, float64x1_t)

/*
** test_vldap1_lane_p64:
** ldap1	{v0\.d}\[0\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1_lane_p64, const poly64_t *, poly64x1_t)

/*
** test_vldap1q_lane_u64:
** ldap1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1q_lane_u64, const uint64_t *, uint64x2_t)

/*
** test_vldap1q_lane_s64:
** ldap1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1q_lane_s64, const int64_t *, int64x2_t)

/*
** test_vldap1q_lane_f64:
** ldap1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1q_lane_f64, const float64_t *, float64x2_t)

/*
** test_vldap1q_lane_p64:
** ldap1	{v0\.d}\[1\], \[x0\]
** ret
*/
TEST_LOAD_LANE (vldap1q_lane_p64, const poly64_t *, poly64x2_t)
