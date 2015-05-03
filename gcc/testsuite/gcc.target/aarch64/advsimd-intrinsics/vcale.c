#define INSN_NAME vcale
#define TEST_MSG "VCALE/VCALEQ"

#include "cmp_fp_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x0, 0xffffffff, 0xffffffff };

VECT_VAR_DECL(expected2,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
