#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vaddl
#define TEST_MSG "VADDL"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = {  0xffe3, 0xffe4, 0xffe5, 0xffe6,
					 0xffe7, 0xffe8, 0xffe9, 0xffea };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe2, 0xffffffe3,
					0xffffffe4, 0xffffffe5 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffe0,
					0xffffffffffffffe1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1e3, 0x1e4, 0x1e5, 0x1e6,
					 0x1e7, 0x1e8, 0x1e9, 0x1ea };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1ffe1, 0x1ffe2,
					 0x1ffe3, 0x1ffe4 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x1ffffffe0, 0x1ffffffe1 };

#include "vXXXl.inc"
