#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmls
#define TEST_MSG "VMLS_LANE"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xc1d9, 0xc1da, 0xc1db, 0xc1dc };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffc1d9, 0xffffc1da };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xc1d9, 0xc1da, 0xc1db, 0xc1dc };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffc1d9, 0xffffc1da };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc420c687, 0xc4208687 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xc1d9, 0xc1da, 0xc1db, 0xc1dc,
					0xc1dd, 0xc1de, 0xc1df, 0xc1e0 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffc1d9, 0xffffc1da,
					0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xc1d9, 0xc1da, 0xc1db, 0xc1dc,
					 0xc1dd, 0xc1de, 0xc1df, 0xc1e0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffc1d9, 0xffffc1da,
					 0xffffc1db, 0xffffc1dc };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc4223168, 0xc421f168,
					   0xc421b168, 0xc4217168 };

#include "vmlX_lane.inc"
