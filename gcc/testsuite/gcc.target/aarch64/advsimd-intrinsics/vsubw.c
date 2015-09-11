#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vsubw
#define TEST_MSG "VSUBW"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfffd, 0xfffe, 0xffff, 0x0,
					0x1, 0x2, 0x3, 0x4 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffe, 0xffffffff, 0x0, 0x1 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x0, 0x1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfefd, 0xfefe, 0xfeff, 0xff00,
					 0xff01, 0xff02, 0xff03, 0xff04 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffeffff, 0xffff0000,
					 0xffff0001, 0xffff0002 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffff00000000,
					 0xffffffff00000001 };

#include "vXXXw.inc"
