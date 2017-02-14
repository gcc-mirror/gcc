#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"


#define INSN_NAME vpmin
#define TEST_MSG "VPMIN"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf2, 0xf4, 0xf6,
				       0xf0, 0xf2, 0xf4, 0xf6 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff2, 0xfff0, 0xfff2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xf2, 0xf4, 0xf6,
					0xf0, 0xf2, 0xf4, 0xf6 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff2, 0xfff0, 0xfff2 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff0 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xcc00, 0xcb00, 0xcc00, 0xcb00 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800000, 0xc1800000 };

#include "vpXXX.inc"
