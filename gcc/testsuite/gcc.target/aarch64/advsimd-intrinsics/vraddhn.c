#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#if defined(__cplusplus)
#include <cstdint>
#else
#include <stdint.h>
#endif

#define INSN_NAME vraddhn
#define TEST_MSG "VRADDHN"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x33, 0x33, 0x33, 0x33,
				       0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x19, 0x19 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x4, 0x4, 0x4, 0x4,
					0x4, 0x4, 0x4, 0x4 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x38, 0x38, 0x38, 0x38 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x4, 0x4 };

#include "vXXXhn.inc"
