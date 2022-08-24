/* { dg-do run } */
/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include <arm_neon.h>

extern void abort();

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
# define ORDER(x, y) y
#else
# define ORDER(x, y) (x - y)
#endif

#define P(n1,n2) n1,n1,n1,n1,n2,n2,n2,n2
#define ARR(nm, p, ty, ...) ty nm##_##p = { __VA_ARGS__ }
#define TEST(t1, t2, t3, f, r1, r2, n1, n2) \
	ARR(f, x, t1, r1);		    \
	ARR(f, y, t2, r2);		    \
	t3 f##_##r = {0};		    \
	f##_##r = f (f##_##r, f##_##x, f##_##y);  \
	if (f##_##r[0] != n1 || f##_##r[1] != n2)   \
	  abort ();

#define TEST_LANE(t1, t2, t3, f, r1, r2, n1, n2, n3, n4) \
	ARR(f, x, t1, r1);		    \
	ARR(f, y, t2, r2);		    \
	t3 f##_##rx = {0};		    \
	f##_##rx = f (f##_##rx, f##_##x, f##_##y, ORDER (1, 0));  \
	if (f##_##rx[0] != n1 || f##_##rx[1] != n2)   \
	  abort ();				    \
	t3 f##_##rx1 = {0};			    \
	f##_##rx1 =  f (f##_##rx1, f##_##x, f##_##y, ORDER (1, 1));  \
	if (f##_##rx1[0] != n3 || f##_##rx1[1] != n4)   \
	  abort ();

#define P2(n1,n2) n1,n1,n1,n1,n2,n2,n2,n2,n1,n1,n1,n1,n2,n2,n2,n2
#define TEST_LANEQ(t1, t2, t3, f, r1, r2, n1, n2, n3, n4) \
	ARR(f, x, t1, r1);		    \
	ARR(f, y, t2, r2);		    \
	t3 f##_##rx = {0};		    \
	f##_##rx = f (f##_##rx, f##_##x, f##_##y, ORDER (3, 2));  \
	if (f##_##rx[0] != n1 || f##_##rx[1] != n2)   \
	  abort ();				    \
	t3 f##_##rx1 = {0};			    \
	f##_##rx1 =  f (f##_##rx1, f##_##x, f##_##y, ORDER (3, 3));  \
	if (f##_##rx1[0] != n3 || f##_##rx1[1] != n4)   \
	  abort ();

int
main()
{
  TEST (uint8x8_t, uint8x8_t, uint32x2_t, vdot_u32, P(1,2), P(2,3), 8, 24);
  TEST (int8x8_t, int8x8_t, int32x2_t, vdot_s32, P(1,2), P(-2,-3), -8, -24);

  TEST (uint8x16_t, uint8x16_t, uint32x4_t, vdotq_u32, P(1,2), P(2,3), 8, 24);
  TEST (int8x16_t, int8x16_t, int32x4_t, vdotq_s32, P(1,2), P(-2,-3), -8, -24);

  TEST_LANE (uint8x8_t, uint8x8_t, uint32x2_t, vdot_lane_u32, P(1,2), P(2,3), 8, 16, 12, 24);
  TEST_LANE (int8x8_t, int8x8_t, int32x2_t, vdot_lane_s32, P(1,2), P(-2,-3), -8, -16, -12, -24);

  TEST_LANE (uint8x16_t, uint8x8_t, uint32x4_t, vdotq_lane_u32, P(1,2), P(2,3), 8, 16, 12, 24);
  TEST_LANE (int8x16_t, int8x8_t, int32x4_t, vdotq_lane_s32, P(1,2), P(-2,-3), -8, -16, -12, -24);

  TEST_LANEQ (uint8x8_t, uint8x16_t, uint32x2_t, vdot_laneq_u32, P(1,2), P2(2,3), 8, 16, 12, 24);
  TEST_LANEQ (int8x8_t, int8x16_t, int32x2_t, vdot_laneq_s32, P(1,2), P2(-2,-3), -8, -16, -12, -24);

  TEST_LANEQ (uint8x16_t, uint8x16_t, uint32x4_t, vdotq_laneq_u32, P2(1,2), P2(2,3), 8, 16, 12, 24);
  TEST_LANEQ (int8x16_t, int8x16_t, int32x4_t, vdotq_laneq_s32, P2(1,2), P2(-2,-3), -8, -16, -12, -24);

  return 0;
}
