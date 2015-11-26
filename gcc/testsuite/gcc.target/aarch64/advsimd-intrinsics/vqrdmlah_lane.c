/* { dg-require-effective-target arm_v8_1a_neon_hw } */
/* { dg-add-options arm_v8_1a_neon } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR (expected_cumulative_sat, int, 16, 4) = 0;
int VECT_VAR (expected_cumulative_sat, int, 32, 2) = 0;
int VECT_VAR (expected_cumulative_sat, int, 16, 8) = 0;
int VECT_VAR (expected_cumulative_sat, int, 32, 4) = 0;

/* Expected results.  */
VECT_VAR_DECL (expected, int, 16, 4) [] = { 0x38d3, 0x38d4, 0x38d5, 0x38d6 };
VECT_VAR_DECL (expected, int, 32, 2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL (expected, int, 16, 8) [] = { 0x006d, 0x006e, 0x006f, 0x0070,
					    0x0071, 0x0072, 0x0073, 0x0074 };
VECT_VAR_DECL (expected, int, 32, 4) [] = { 0xfffffff0, 0xfffffff1,
					    0xfffffff2, 0xfffffff3 };

/* Expected values of cumulative_saturation flag when multiplication
   saturates.  */
int VECT_VAR (expected_cumulative_sat_mul, int, 16, 4) = 0;
int VECT_VAR (expected_cumulative_sat_mul, int, 32, 2) = 0;
int VECT_VAR (expected_cumulative_sat_mul, int, 16, 8) = 0;
int VECT_VAR (expected_cumulative_sat_mul, int, 32, 4) = 0;

/* Expected results when multiplication saturates.  */
VECT_VAR_DECL (expected_mul, int, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_mul, int, 32, 2) [] = { 0x0, 0x0 };
VECT_VAR_DECL (expected_mul, int, 16, 8) [] = { 0x0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_mul, int, 32, 4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected values of cumulative_saturation flag when rounding
   should not cause saturation.  */
int VECT_VAR (expected_cumulative_sat_round, int, 16, 4) = 0;
int VECT_VAR (expected_cumulative_sat_round, int, 32, 2) = 0;
int VECT_VAR (expected_cumulative_sat_round, int, 16, 8) = 0;
int VECT_VAR (expected_cumulative_sat_round, int, 32, 4) = 0;

/* Expected results when rounding should not cause saturation.  */
VECT_VAR_DECL (expected_round, int, 16, 4) [] = { 0xfffe, 0xfffe,
						  0xfffe, 0xfffe };
VECT_VAR_DECL (expected_round, int, 32, 2) [] = { 0xfffffffe, 0xfffffffe };
VECT_VAR_DECL (expected_round,int, 16, 8) [] = { 0xfffe, 0xfffe,
						 0xfffe, 0xfffe,
						 0xfffe, 0xfffe,
						 0xfffe, 0xfffe };
VECT_VAR_DECL (expected_round, int, 32, 4) [] = { 0xfffffffe, 0xfffffffe,
						  0xfffffffe, 0xfffffffe };

#define INSN vqrdmlah
#define TEST_MSG "VQRDMLAH_LANE"

#include "vqrdmlXh_lane.inc"
