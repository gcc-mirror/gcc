#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vaddw
#define TEST_MSG "VADDW"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = {  0xffe3, 0xffe4, 0xffe5, 0xffe6,
					 0xffe7, 0xffe8, 0xffe9, 0xffea };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe2, 0xffffffe3,
					0xffffffe4, 0xffffffe5 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffe0,
					0xffffffffffffffe1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xe3, 0xe4, 0xe5, 0xe6,
					 0xe7, 0xe8, 0xe9, 0xea };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffe1, 0xffe2,
					 0xffe3, 0xffe4 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffe0, 0xffffffe1 };

#include "vXXXw.inc"
