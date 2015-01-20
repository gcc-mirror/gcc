#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlsl
#define TEST_MSG "VMLSL"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0x16d9, 0x16da, 0x16db, 0x16dc,
					0x16dd, 0x16de, 0x16df, 0x16e0 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffc1d9, 0xffffc1da,
					0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffc1d9,
					0xffffffffffffc1da };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xc1d9, 0xc1da, 0xc1db, 0xc1dc,
					 0xc1dd, 0xc1de, 0xc1df, 0xc1e0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffc1d9, 0xffffc1da,
					 0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffc1d9,
					 0xffffffffffffc1da };

#include "vmlXl.inc"
