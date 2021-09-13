#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vqdmlsl_n
#define TEST_MSG "VQDMLSL_N"

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffe95c, 0xffffe95d,
					0xffffe95e, 0xffffe95f };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffde12,
					0xffffffffffffde13 };

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected2,int,32,4) [] = { 0x80000000, 0x80000000,
					 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0x8000000000000000,
					 0x8000000000000000 };

#include "vqdmlXl_n.inc"
