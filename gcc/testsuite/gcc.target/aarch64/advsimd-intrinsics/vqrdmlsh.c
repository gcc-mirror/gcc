/* { dg-require-effective-target arm_v8_1a_neon_hw } */
/* { dg-add-options arm_v8_1a_neon } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL (expected, int, 16, 4) [] = { 0xc70d, 0xc70e, 0xc70f, 0xc710 };
VECT_VAR_DECL (expected, int, 32, 2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL (expected, int, 16, 8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					    0xfff4, 0xfff5, 0xfff6, 0xfff7 };
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
#define TEST_MSG "VQRDMLSH"

#include "vqrdmlXh.inc"
