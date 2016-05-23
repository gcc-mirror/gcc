/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-add-options arm_v8_neon } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL (expected, hfloat, 32, 2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL (expected, hfloat, 32, 4) [] = { 0xc1800000, 0xc1700000,
					       0xc1600000, 0xc1500000 };

#define INSN vrndm
#define TEST_MSG "VRNDM"

#include "vrndX.inc"
