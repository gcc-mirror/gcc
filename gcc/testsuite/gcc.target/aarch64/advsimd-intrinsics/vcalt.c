#define INSN_NAME vcalt
#define TEST_MSG "VCALT/VCALTQ"

#include "cmp_fp_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x0, 0xffffffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x0, 0x0, 0xffffffff };

VECT_VAR_DECL(expected2,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected, uint, 16, 4) [] = { 0x0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL (expected, uint, 16, 8) [] = { 0x0, 0x0, 0x0, 0xffff,
					     0xffff, 0xffff, 0xffff, 0xffff };

VECT_VAR_DECL (expected2, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected2, uint, 16, 8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0xffff };
#endif
