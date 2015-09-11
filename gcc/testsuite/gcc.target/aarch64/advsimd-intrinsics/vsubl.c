#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vsubl
#define TEST_MSG "VSUBL"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfffd, 0xfffe, 0xffff, 0x0,
					0x1, 0x2, 0x3, 0x4 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffe, 0xffffffff, 0x0, 0x1 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x0, 0x1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfffd, 0xfffe, 0xffff, 0x0,
					 0x1, 0x2, 0x3, 0x4 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffff, 0x0, 0x1, 0x2 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x0, 0x1 };

#include "vXXXl.inc"
