#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"


#define INSN_NAME vpmax
#define TEST_MSG "VPMAX"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf1, 0xf3, 0xf5, 0xf7,
				       0xf1, 0xf3, 0xf5, 0xf7 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff1, 0xfff3, 0xfff1, 0xfff3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf1, 0xf3, 0xf5, 0xf7,
					0xf1, 0xf3, 0xf5, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff1, 0xfff3, 0xfff1, 0xfff3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff1, 0xfffffff1 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xcb80, 0xca80, 0xcb80, 0xca80 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1700000, 0xc1700000 };

#include "vpXXX.inc"
