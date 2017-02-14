#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vpadd
#define TEST_MSG "VPADD"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xe1, 0xe5, 0xe9, 0xed,
				       0xe1, 0xe5, 0xe9, 0xed };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffe1, 0xffe5, 0xffe1, 0xffe5 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffe1, 0xffffffe1 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xe1, 0xe5, 0xe9, 0xed,
					0xe1, 0xe5, 0xe9, 0xed };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffe1, 0xffe5, 0xffe1, 0xffe5 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffe1, 0xffffffe1 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xcfc0, 0xcec0, 0xcfc0, 0xcec0 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1f80000, 0xc1f80000 };

#include "vpXXX.inc"
