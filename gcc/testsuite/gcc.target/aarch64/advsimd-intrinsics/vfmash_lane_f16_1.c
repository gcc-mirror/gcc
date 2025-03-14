/* { dg-require-effective-target arm_v8_2a_fp16_scalar_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A0 FP16_C (123.4)
#define B0 FP16_C (-5.8)
#define C0 FP16_C (-3.8)
#define D0 FP16_C (10)

#define A1 FP16_C (12.4)
#define B1 FP16_C (-5.8)
#define C1 FP16_C (90.8)
#define D1 FP16_C (24)

#define A2 FP16_C (23.4)
#define B2 FP16_C (-5.8)
#define C2 FP16_C (8.9)
#define D2 FP16_C (4)

#define E0 FP16_C (3.4)
#define F0 FP16_C (-55.8)
#define G0 FP16_C (-31.8)
#define H0 FP16_C (2)

#define E1 FP16_C (123.4)
#define F1 FP16_C (-5.8)
#define G1 FP16_C (-3.8)
#define H1 FP16_C (102)

#define E2 FP16_C (4.9)
#define F2 FP16_C (-15.8)
#define G2 FP16_C (39.8)
#define H2 FP16_C (49)

extern void abort ();

float16_t src1[8] = { A0, B0, C0, D0, E0, F0, G0, H0 };
float16_t src2[8] = { A1, B1, C1, D1, E1, F1, G1, H1 };
VECT_VAR_DECL (src3, float, 16, 4) [] = { A2, B2, C2, D2 };
VECT_VAR_DECL (src3, float, 16, 8) [] = { A2, B2, C2, D2, E2, F2, G2, H2 };

/* Expected results for vfmah_lane_f16.  */
uint16_t expected[4] = { 0x5E76 /* A0 + A1 * A2.  */,
			 0x4EF6 /* B0 + B1 * B2.  */,
			 0x6249 /* C0 + C1 * C2.  */,
			 0x56A0 /* D0 + D1 * D2.  */ };

/* Expected results for vfmah_laneq_f16.  */
uint16_t expected_laneq[8] = { 0x5E76 /* A0 + A1 * A2.  */,
			       0x4EF6 /* B0 + B1 * B2.  */,
			       0x6249 /* C0 + C1 * C2.  */,
			       0x56A0 /* D0 + D1 * D2.  */,
			       0x60BF /* E0 + E1 * E2.  */,
			       0x507A /* F0 + F1 * F2.  */,
			       0xD9B9 /* G0 + G1 * G2.  */,
			       0x6CE2 /* H0 + H1 * H2.  */ };

/* Expected results for vfmsh_lane_f16.  */
uint16_t expected_fms[4] = { 0xD937 /* A0 + -A1 * A2.  */,
			     0xD0EE /* B0 + -B1 * B2.  */,
			     0xE258 /* C0 + -C1 * C2.  */,
			     0xD560 /* D0 + -D1 * D2.  */ };

/* Expected results for vfmsh_laneq_f16.  */
uint16_t expected_fms_laneq[8] = { 0xD937 /* A0 + -A1 * A2.  */,
				   0xD0EE /* B0 + -B1 * B2.  */,
				   0xE258 /* C0 + -C1 * C2.  */,
				   0xD560 /* D0 + -D1 * D2.  */,
				   0xE0B2 /* E0 + -E1 * E2.  */,
				   0xD89C /* F0 + -F1 * F2.  */,
				   0x5778 /* G0 + -G1 * G2.  */,
				   0xECE1 /* H0 + -H1 * H2.  */ };

void exec_vfmash_lane_f16 (void)
{
#define CHECK_LANE(N) \
  ret = vfmah_lane_f16 (src1[N], src2[N], VECT_VAR (vsrc3, float, 16, 4), N);\
  if (*(uint16_t *) &ret != expected[N])\
    abort ();

  DECL_VARIABLE(vsrc3, float, 16, 4);
  VLOAD (vsrc3, src3, , float, f, 16, 4);
  float16_t ret;
  CHECK_LANE(0)
  CHECK_LANE(1)
  CHECK_LANE(2)
  CHECK_LANE(3)

#undef CHECK_LANE
#define CHECK_LANE(N) \
  ret = vfmah_laneq_f16 (src1[N], src2[N], VECT_VAR (vsrc3, float, 16, 8), N);\
  if (*(uint16_t *) &ret != expected_laneq[N]) \
	  abort ();

  DECL_VARIABLE(vsrc3, float, 16, 8);
  VLOAD (vsrc3, src3, q, float, f, 16, 8);
  CHECK_LANE(0)
  CHECK_LANE(1)
  CHECK_LANE(2)
  CHECK_LANE(3)
  CHECK_LANE(4)
  CHECK_LANE(5)
  CHECK_LANE(6)
  CHECK_LANE(7)

#undef CHECK_LANE
#define CHECK_LANE(N) \
  ret = vfmsh_lane_f16 (src1[N], src2[N], VECT_VAR (vsrc3, float, 16, 4), N);\
  if (*(uint16_t *) &ret != expected_fms[N])\
    abort ();

  CHECK_LANE(0)
  CHECK_LANE(1)
  CHECK_LANE(2)

#undef CHECK_LANE
#define CHECK_LANE(N) \
  ret = vfmsh_laneq_f16 (src1[N], src2[N], VECT_VAR (vsrc3, float, 16, 8), N);\
  if (*(uint16_t *) &ret != expected_fms_laneq[N]) \
	  abort ();

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
  exec_vfmash_lane_f16 ();
  return 0;
}
