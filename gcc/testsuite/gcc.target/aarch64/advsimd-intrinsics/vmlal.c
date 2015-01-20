#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlal
#define TEST_MSG "VMLAL"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xe907, 0xe908, 0xe909, 0xe90a,
					0xe90b, 0xe90c, 0xe90d, 0xe90e };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3e07, 0x3e08 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a,
					 0x3e0b, 0x3e0c, 0x3e0d, 0x3e0e };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3e07, 0x3e08 };

#include "vmlXl.inc"
