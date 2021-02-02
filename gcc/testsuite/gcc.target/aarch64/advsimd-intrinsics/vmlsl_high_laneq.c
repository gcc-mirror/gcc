/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlsl_high_laneq
#define TEST_MSG "VMLSL_HIGH_LANEQ"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0xffffc1d9, 0xffffc1da,
					   0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0xffffffffffffc1d9,
					   0xffffffffffffc1da };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0xffffc1d9, 0xffffc1da,
					    0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0xffffffffffffc1d9,
					    0xffffffffffffc1da };

#include "vmlXl_high_laneq.inc"
