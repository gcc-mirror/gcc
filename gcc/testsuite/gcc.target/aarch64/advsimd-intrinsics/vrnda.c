/* { dg-require-effective-target arm_v8_neon_hw } */
/* { dg-add-options arm_v8_neon } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
/* Expected results.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected, hfloat, 16, 4) [] = { 0xcc00, 0xcb80,
					       0xcb00, 0xca80 };
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xcc00, 0xcb80,
					       0xcb00, 0xca80,
					       0xca00, 0xc980,
					       0xc900, 0xc880 };
#endif
VECT_VAR_DECL (expected, hfloat, 32, 2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL (expected, hfloat, 32, 4) [] = { 0xc1800000, 0xc1700000,
					       0xc1600000, 0xc1500000 };

#define INSN vrnda
#define TEST_MSG "VRNDA"

#include "vrndX.inc"
