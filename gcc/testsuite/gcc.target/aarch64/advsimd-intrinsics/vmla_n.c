#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmla
#define TEST_MSG "VMLA_N"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0x595, 0x596, 0x597, 0x598 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xb3a, 0xb3b };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x10df, 0x10e0, 0x10e1, 0x10e2 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x1684, 0x1685 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x4497deb8, 0x4497feb8 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x1c29, 0x1c2a, 0x1c2b, 0x1c2c,
					0x1c2d, 0x1c2e, 0x1c2f, 0x1c30 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x21ce, 0x21cf, 0x21d0, 0x21d1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x2773, 0x2774, 0x2775, 0x2776,
					 0x2777, 0x2778, 0x2779, 0x277a };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x2d18, 0x2d19, 0x2d1a, 0x2d1b };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x4568087b, 0x4568187b,
					   0x4568287b, 0x4568387b };

#include "vmlX_n.inc"
