/* { dg-require-effective-target arm_v8_1a_neon_hw } */
/* { dg-add-options arm_v8_1a_neon } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL (expected, int, 16, 4) [] = { 0xc70d, 0xc70e, 0xc70f, 0xc710 };
VECT_VAR_DECL (expected, int, 32, 2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL (expected, int, 16, 8) [] = { 0xff73, 0xff74, 0xff75, 0xff76,
					    0xff77, 0xff78, 0xff79, 0xff7a };
VECT_VAR_DECL (expected, int, 32, 4) [] = { 0xfffffff0, 0xfffffff1,
					    0xfffffff2, 0xfffffff3 };

/* Expected results when multiplication saturates.  */
VECT_VAR_DECL (expected_mul, int, 16, 4) [] = { 0x8000, 0x8000,
						0x8000, 0x8000 };
VECT_VAR_DECL (expected_mul, int, 32, 2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL (expected_mul, int, 16, 8) [] = { 0x8000, 0x8000,
						0x8000, 0x8000,
						0x8000, 0x8000,
						0x8000, 0x8000 };
VECT_VAR_DECL (expected_mul, int, 32, 4) [] = { 0x80000000, 0x80000000,
						0x80000000, 0x80000000 };

/* Expected results when rounding should not cause saturation.  */
VECT_VAR_DECL (expected_round, int, 16, 4) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL (expected_round, int, 32, 2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL (expected_round, int, 16, 8) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL (expected_round, int, 32, 4) [] = { 0x80000000, 0x80000000,
						  0x80000000, 0x80000000 };

#define INSN vqrdmlsh
#define TEST_MSG "VQRDMLSH_LANE"

#include "vqrdmlXh_lane.inc"
