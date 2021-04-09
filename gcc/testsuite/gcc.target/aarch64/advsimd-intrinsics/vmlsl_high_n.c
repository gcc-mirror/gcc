/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlsl_high_n
#define TEST_MSG "VMLSL_HIGH_N"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0xfffffa4b, 0xfffffa4c,
					   0xfffffa4d, 0xfffffa4e };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0xfffffffffffff4a6,
					   0xfffffffffffff4a7 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0xffffef01, 0xffffef02,
					    0xffffef03, 0xffffef04 };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0xffffffffffffef01,
					    0xffffffffffffef02 };

#include "vmlXl_high_n.inc"
