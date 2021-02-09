/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vpaddq
#define TEST_MSG "VPADDQ"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 8, 16) [] = { 0xe1, 0xe5, 0xe9, 0xed,
					   0xf1, 0xf5, 0xf9, 0xfd,
					   0xe1, 0xe5, 0xe9, 0xed,
					   0xf1, 0xf5, 0xf9, 0xfd };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0xffe1, 0xffe5, 0xffe9, 0xffed,
					   0xffe1, 0xffe5, 0xffe9, 0xffed };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0xffffffe1, 0xffffffe5,
					   0xffffffe1, 0xffffffe5 };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0xffffffffffffffe1,
					   0xffffffffffffffe1 };
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0xe1, 0xe5, 0xe9, 0xed,
					    0xf1, 0xf5, 0xf9, 0xfd,
					    0xe1, 0xe5, 0xe9, 0xed,
					    0xf1, 0xf5, 0xf9, 0xfd };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0xffe1, 0xffe5, 0xffe9, 0xffed,
					    0xffe1, 0xffe5, 0xffe9, 0xffed };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0xffffffe1, 0xffffffe5,
					    0xffffffe1, 0xffffffe5};
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0xffffffffffffffe1,
					    0xffffffffffffffe1 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xcfc0, 0xcec0, 0xcdc0, 0xccc0,
					      0xcfc0, 0xcec0, 0xcdc0, 0xccc0 };
#endif
VECT_VAR_DECL(expected, hfloat, 32, 4) [] = { 0xc1f80000, 0xc1d80000,
					      0xc1f80000, 0xc1d80000 };
VECT_VAR_DECL(expected, hfloat, 64, 2) [] = { 0xc03f000000000000,
					      0xc03f000000000000 };

#include "vpXXXq.inc"
