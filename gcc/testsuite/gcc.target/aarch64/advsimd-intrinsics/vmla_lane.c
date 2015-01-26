#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmla
#define TEST_MSG "VMLA_LANE"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x3e07, 0x3e08 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x3e07, 0x3e08 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x4418c687, 0x44190687 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a,
					0x3e0b, 0x3e0c, 0x3e0d, 0x3e0e };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a,
					 0x3e0b, 0x3e0c, 0x3e0d, 0x3e0e };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x3e07, 0x3e08, 0x3e09, 0x3e0a };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x441a3168, 0x441a7168,
					   0x441ab168, 0x441af168 };

#include "vmlX_lane.inc"
