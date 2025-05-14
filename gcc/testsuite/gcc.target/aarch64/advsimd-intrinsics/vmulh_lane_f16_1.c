/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A FP16_C (13.4)
#define B FP16_C (-56.8)
#define C FP16_C (-34.8)
#define D FP16_C (12)
#define E FP16_C (63.1)
#define F FP16_C (19.1)
#define G FP16_C (-4.8)
#define H FP16_C (77)

#define I FP16_C (0.7)
#define J FP16_C (-78)
#define K FP16_C (11.23)
#define L FP16_C (98)
#define M FP16_C (87.1)
#define N FP16_C (-8)
#define O FP16_C (-1.1)
#define P FP16_C (-9.7)

extern void abort ();

float16_t src1[8] = { A, B, C, D, I, J, K, L };
VECT_VAR_DECL (src2, float, 16, 4) [] = { E, F, G, H };
VECT_VAR_DECL (src2, float, 16, 8) [] = { E, F, G, H, M, N, O, P };

/* Expected results for vmulh_lane.  */
uint16_t expected[4] = { 0x629B /* A * E.  */, 0xE43D /* B * F.  */,
			 0x5939 /* C * G.  */, 0x6338 /* D * H.  */ };


/* Expected results for vmulh_lane.  */
uint16_t expected_laneq[8] = { 0x629B /* A * E.  */,
			       0xE43D /* B * F.  */,
			       0x5939 /* C * G.  */,
			       0x6338 /* D * H.  */,
			       0x53A0 /* I * M.  */,
			       0x60E0 /* J * N.  */,
			       0xCA2C /* K * O.  */,
			       0xE36E /* L * P.  */ };

void exec_vmulh_lane_f16 (void)
{
#define CHECK_LANE(N)\
  ret = vmulh_lane_f16 (src1[N], VECT_VAR (vsrc2, float, 16, 4), N);\
  if (*(uint16_t *) &ret != expected[N])\
    abort ();

  DECL_VARIABLE(vsrc2, float, 16, 4);
  VLOAD (vsrc2, src2, , float, f, 16, 4);
  float16_t ret;

  CHECK_LANE(0)
  CHECK_LANE(1)
  CHECK_LANE(2)
  CHECK_LANE(3)

#undef CHECK_LANE
#define CHECK_LANE(N)\
  ret = vmulh_laneq_f16 (src1[N], VECT_VAR (vsrc2, float, 16, 8), N);\
  if (*(uint16_t *) &ret != expected_laneq[N])\
    abort ();

  DECL_VARIABLE(vsrc2, float, 16, 8);
  VLOAD (vsrc2, src2, q, float, f, 16, 8);

  CHECK_LANE(0)
  CHECK_LANE(1)
  CHECK_LANE(2)
  CHECK_LANE(3)
  CHECK_LANE(4)
  CHECK_LANE(5)
  CHECK_LANE(6)
  CHECK_LANE(7)
}

int
main (void)
{
  exec_vmulh_lane_f16 ();
  return 0;
}
