/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlal_high_laneq
#define TEST_MSG "VMLAL_HIGH_LANEQ"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0x3e07, 0x3e08 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0x3e07, 0x3e08 };

#include "vmlXl_high_laneq.inc"
