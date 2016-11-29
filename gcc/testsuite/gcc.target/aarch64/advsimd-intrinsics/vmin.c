#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmin
#define TEST_MSG "VMIN/VMINQ"

#define HAS_FLOAT_VARIANT

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
#define HAS_FLOAT16_VARIANT
#endif

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
				       0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff0 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 4) [] = { 0xcc00, 0xcbc0, 0xcbc0, 0xcbc0 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800000, 0xc1780000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf4, 0xf4, 0xf4,
					0xf4, 0xf4, 0xf4, 0xf4,
					0xf4, 0xf4, 0xf4, 0xf4 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff3, 0xfff3, 0xfff3, 0xfff3 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xf8, 0xf9, 0xf9, 0xf9,
					 0xf9, 0xf9, 0xf9, 0xf9 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff2,
					 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, hfloat, 16, 8) [] = { 0xcc00, 0xcb80, 0xcb40, 0xcb40,
					      0xcb40, 0xcb40, 0xcb40, 0xcb40 };
#endif
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					   0xc1680000, 0xc1680000 };
/* Expected results with special FP values.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_nan, hfloat, 16, 8) [] = { 0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00,
						  0x7e00, 0x7e00 };
VECT_VAR_DECL(expected_mnan, hfloat, 16, 8) [] = { 0x7e00, 0x7e00,
						   0x7e00, 0x7e00,
						   0x7e00, 0x7e00,
						   0x7e00, 0x7e00 };
VECT_VAR_DECL(expected_inf, hfloat, 16, 8) [] = { 0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00,
						  0x3c00, 0x3c00 };
VECT_VAR_DECL(expected_minf, hfloat, 16, 8) [] = { 0xfc00, 0xfc00,
						   0xfc00, 0xfc00,
						   0xfc00, 0xfc00,
						   0xfc00, 0xfc00 };
VECT_VAR_DECL(expected_zero1, hfloat, 16, 8) [] = { 0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000 };
VECT_VAR_DECL(expected_zero2, hfloat, 16, 8) [] = { 0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000,
						    0x8000, 0x8000 };
#endif
VECT_VAR_DECL(expected_nan,hfloat,32,4) [] = { 0x7fc00000, 0x7fc00000,
					       0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_mnan,hfloat,32,4) [] = { 0x7fc00000, 0x7fc00000,
						0x7fc00000, 0x7fc00000 };
VECT_VAR_DECL(expected_inf,hfloat,32,4) [] = { 0x3f800000, 0x3f800000,
					       0x3f800000, 0x3f800000 };
VECT_VAR_DECL(expected_minf,hfloat,32,4) [] = { 0xff800000, 0xff800000,
						0xff800000, 0xff800000 };
VECT_VAR_DECL(expected_zero1,hfloat,32,4) [] = { 0x80000000, 0x80000000,
						 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_zero2,hfloat,32,4) [] = { 0x80000000, 0x80000000,
						 0x80000000, 0x80000000 };

#include "binary_op_no64.inc"
