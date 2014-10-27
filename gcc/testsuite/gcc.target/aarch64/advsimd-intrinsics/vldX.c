#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */

/* vld2/chunk 0.  */
VECT_VAR_DECL(expected_vld2_0,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					      0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld2_0,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld2_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld2_0,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld2_0,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld2_0,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld2_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld2_0,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld2_0,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld2_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld2_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld2_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_0,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld2_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_0,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld2_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						  0xc1600000, 0xc1500000 };

/* vld2/chunk 1.  */
VECT_VAR_DECL(expected_vld2_1,int,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					      0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_1,int,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_1,int,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_1,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld2_1,uint,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_1,uint,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_1,uint,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_1,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld2_1,poly,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_1,poly,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld2_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld2_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld2_1,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld2_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld2_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld2_1,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld2_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld2_1,hfloat,32,4) [] = { 0xc1400000, 0xc1300000,
						  0xc1200000, 0xc1100000 };

/* vld3/chunk 0.  */
VECT_VAR_DECL(expected_vld3_0,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					      0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld3_0,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld3_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld3_0,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld3_0,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld3_0,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld3_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld3_0,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld3_0,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld3_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld3_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld3_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_0,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_0,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						  0xc1600000, 0xc1500000 };

/* vld3/chunk 1.  */
VECT_VAR_DECL(expected_vld3_1,int,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					      0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_1,int,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_1,int,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_1,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld3_1,uint,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_1,uint,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_1,uint,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_1,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld3_1,poly,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_1,poly,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld3_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld3_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld3_1,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld3_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld3_1,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld3_1,hfloat,32,4) [] = { 0xc1400000, 0xc1300000,
						  0xc1200000, 0xc1100000 };

/* vld3/chunk 2.  */
VECT_VAR_DECL(expected_vld3_2,int,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					      0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,int,16,4) [] = { 0xfff8, 0xfff9,
					       0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld3_2,int,32,2) [] = { 0xfffffff4, 0xfffffff5 };
VECT_VAR_DECL(expected_vld3_2,int,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld3_2,uint,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,uint,16,4) [] = { 0xfff8, 0xfff9,
						0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld3_2,uint,32,2) [] = { 0xfffffff4, 0xfffffff5 };
VECT_VAR_DECL(expected_vld3_2,uint,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld3_2,poly,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,poly,16,4) [] = { 0xfff8, 0xfff9,
						0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld3_2,hfloat,32,2) [] = { 0xc1400000, 0xc1300000 };
VECT_VAR_DECL(expected_vld3_2,int,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
					       0x14, 0x15, 0x16, 0x17,
					       0x18, 0x19, 0x1a, 0x1b,
					       0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,int,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,int,32,4) [] = { 0xfffffff8, 0xfffffff9,
					       0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld3_2,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_2,uint,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,uint,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,uint,32,4) [] = { 0xfffffff8, 0xfffffff9,
						0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld3_2,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld3_2,poly,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,poly,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,hfloat,32,4) [] = { 0xc1000000, 0xc0e00000,
						  0xc0c00000, 0xc0a00000 };

/* vld4/chunk 0.  */
VECT_VAR_DECL(expected_vld4_0,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					      0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld4_0,int,16,4) [] = { 0xfff0, 0xfff1,
					       0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld4_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld4_0,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld4_0,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld4_0,uint,16,4) [] = { 0xfff0, 0xfff1,
						0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld4_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_vld4_0,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld4_0,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_vld4_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld4_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld4_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_0,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_0,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						  0xc1600000, 0xc1500000 };

/* vld4/chunk 1.  */
VECT_VAR_DECL(expected_vld4_1,int,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					      0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_1,int,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_1,int,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_1,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld4_1,uint,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_1,uint,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_1,uint,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_1,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld4_1,poly,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_1,poly,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld4_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_1,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_1,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_1,hfloat,32,4) [] = { 0xc1400000, 0xc1300000,
						  0xc1200000, 0xc1100000 };

/* vld4/chunk 2.  */
VECT_VAR_DECL(expected_vld4_2,int,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					      0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,int,16,4) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld4_2,int,32,2) [] = { 0xfffffff4, 0xfffffff5 };
VECT_VAR_DECL(expected_vld4_2,int,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld4_2,uint,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,uint,16,4) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld4_2,uint,32,2) [] = { 0xfffffff4, 0xfffffff5 };
VECT_VAR_DECL(expected_vld4_2,uint,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld4_2,poly,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,poly,16,4) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb };
VECT_VAR_DECL(expected_vld4_2,hfloat,32,2) [] = { 0xc1400000, 0xc1300000 };
VECT_VAR_DECL(expected_vld4_2,int,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
					       0x14, 0x15, 0x16, 0x17,
					       0x18, 0x19, 0x1a, 0x1b,
					       0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,int,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,int,32,4) [] = { 0xfffffff8, 0xfffffff9,
					       0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld4_2,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_2,uint,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,uint,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,uint,32,4) [] = { 0xfffffff8, 0xfffffff9,
						0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld4_2,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_2,poly,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,poly,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,hfloat,32,4) [] = { 0xc1000000, 0xc0e00000,
						  0xc0c00000, 0xc0a00000 };

/* vld4/chunk 3.  */
VECT_VAR_DECL(expected_vld4_3,int,8,8) [] = { 0x8, 0x9, 0xa, 0xb,
					      0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,int,16,4) [] = { 0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_3,int,32,2) [] = { 0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_3,int,64,1) [] = { 0xfffffffffffffff3 };
VECT_VAR_DECL(expected_vld4_3,uint,8,8) [] = { 0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,uint,16,4) [] = { 0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_3,uint,32,2) [] = { 0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_3,uint,64,1) [] = { 0xfffffffffffffff3 };
VECT_VAR_DECL(expected_vld4_3,poly,8,8) [] = { 0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,poly,16,4) [] = { 0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_3,hfloat,32,2) [] = { 0xc1200000, 0xc1100000 };
VECT_VAR_DECL(expected_vld4_3,int,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
					       0x24, 0x25, 0x26, 0x27,
					       0x28, 0x29, 0x2a, 0x2b,
					       0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,int,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,int,32,4) [] = { 0xfffffffc, 0xfffffffd,
					       0xfffffffe, 0xffffffff };
VECT_VAR_DECL(expected_vld4_3,int,64,2) [] = { 0x3333333333333333,
					       0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_3,uint,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
						0x24, 0x25, 0x26, 0x27,
						0x28, 0x29, 0x2a, 0x2b,
						0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,uint,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,uint,32,4) [] = { 0xfffffffc, 0xfffffffd,
						0xfffffffe, 0xffffffff };
VECT_VAR_DECL(expected_vld4_3,uint,64,2) [] = { 0x3333333333333333,
						0x3333333333333333 };
VECT_VAR_DECL(expected_vld4_3,poly,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
						0x24, 0x25, 0x26, 0x27,
						0x28, 0x29, 0x2a, 0x2b,
						0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,poly,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,hfloat,32,4) [] = { 0xc0800000, 0xc0400000,
						  0xc0000000, 0xbf800000 };

void exec_vldX (void)
{
  /* In this case, input variables are arrays of vectors.  */
#define DECL_VLDX(T1, W, N, X)						\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector, T1, W, N, X);	\
  VECT_VAR_DECL(result_bis_##X, T1, W, N)[X * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behaviour. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
#define TEST_VLDX(Q, T1, T2, W, N, X)					\
  VECT_ARRAY_VAR(vector, T1, W, N, X) =					\
    /* Use dedicated init buffer, of size X */				\
    vld##X##Q##_##T2##W(VECT_ARRAY_VAR(buffer_vld##X, T1, W, N, X));	\
  vst##X##Q##_##T2##W(VECT_VAR(result_bis_##X, T1, W, N),		\
		      VECT_ARRAY_VAR(vector, T1, W, N, X));		\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[Y].  */
#define TEST_EXTRA_CHUNK(T1, W, N, X,Y)			\
  memcpy(VECT_VAR(result, T1, W, N),			\
	 &(VECT_VAR(result_bis_##X, T1, W, N)[Y*N]),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* We need all variants in 64 bits, but there is no 64x2 variant.  */
#define DECL_ALL_VLDX(X)			\
  DECL_VLDX(int, 8, 8, X);			\
  DECL_VLDX(int, 16, 4, X);			\
  DECL_VLDX(int, 32, 2, X);			\
  DECL_VLDX(int, 64, 1, X);			\
  DECL_VLDX(uint, 8, 8, X);			\
  DECL_VLDX(uint, 16, 4, X);			\
  DECL_VLDX(uint, 32, 2, X);			\
  DECL_VLDX(uint, 64, 1, X);			\
  DECL_VLDX(poly, 8, 8, X);			\
  DECL_VLDX(poly, 16, 4, X);			\
  DECL_VLDX(float, 32, 2, X);			\
  DECL_VLDX(int, 8, 16, X);			\
  DECL_VLDX(int, 16, 8, X);			\
  DECL_VLDX(int, 32, 4, X);			\
  DECL_VLDX(uint, 8, 16, X);			\
  DECL_VLDX(uint, 16, 8, X);			\
  DECL_VLDX(uint, 32, 4, X);			\
  DECL_VLDX(poly, 8, 16, X);			\
  DECL_VLDX(poly, 16, 8, X);			\
  DECL_VLDX(float, 32, 4, X)

#define TEST_ALL_VLDX(X)			\
  TEST_VLDX(, int, s, 8, 8, X);			\
  TEST_VLDX(, int, s, 16, 4, X);		\
  TEST_VLDX(, int, s, 32, 2, X);		\
  TEST_VLDX(, int, s, 64, 1, X);		\
  TEST_VLDX(, uint, u, 8, 8, X);		\
  TEST_VLDX(, uint, u, 16, 4, X);		\
  TEST_VLDX(, uint, u, 32, 2, X);		\
  TEST_VLDX(, uint, u, 64, 1, X);		\
  TEST_VLDX(, poly, p, 8, 8, X);		\
  TEST_VLDX(, poly, p, 16, 4, X);		\
  TEST_VLDX(, float, f, 32, 2, X);		\
  TEST_VLDX(q, int, s, 8, 16, X);		\
  TEST_VLDX(q, int, s, 16, 8, X);		\
  TEST_VLDX(q, int, s, 32, 4, X);		\
  TEST_VLDX(q, uint, u, 8, 16, X);		\
  TEST_VLDX(q, uint, u, 16, 8, X);		\
  TEST_VLDX(q, uint, u, 32, 4, X);		\
  TEST_VLDX(q, poly, p, 8, 16, X);		\
  TEST_VLDX(q, poly, p, 16, 8, X);		\
  TEST_VLDX(q, float, f, 32, 4, X)

#define TEST_ALL_EXTRA_CHUNKS(X, Y)		\
  TEST_EXTRA_CHUNK(int, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(int, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(int, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(int, 64, 1, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 64, 1, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(float, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(int, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(int, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(int, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(float, 32, 4, X, Y)

  DECL_ALL_VLDX(2);
  DECL_ALL_VLDX(3);
  DECL_ALL_VLDX(4);

  /* Special input buffers of suitable size are needed for vld2/vld3/vld4.  */
  /* Input buffers for vld2, 1 of each size */
  VECT_ARRAY_INIT2(buffer_vld2, int, 8, 8);
  PAD(buffer_vld2_pad, int, 8, 8);
  VECT_ARRAY_INIT2(buffer_vld2, int, 16, 4);
  PAD(buffer_vld2_pad, int, 16, 4);
  VECT_ARRAY_INIT2(buffer_vld2, int, 32, 2);
  PAD(buffer_vld2_pad, int, 32, 2);
  VECT_ARRAY_INIT2(buffer_vld2, int, 64, 1);
  PAD(buffer_vld2_pad, int, 64, 1);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 8, 8);
  PAD(buffer_vld2_pad, uint, 8, 8);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 16, 4);
  PAD(buffer_vld2_pad, uint, 16, 4);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 32, 2);
  PAD(buffer_vld2_pad, uint, 32, 2);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 64, 1);
  PAD(buffer_vld2_pad, uint, 64, 1);
  VECT_ARRAY_INIT2(buffer_vld2, poly, 8, 8);
  PAD(buffer_vld2_pad, poly, 8, 8);
  VECT_ARRAY_INIT2(buffer_vld2, poly, 16, 4);
  PAD(buffer_vld2_pad, poly, 16, 4);
  VECT_ARRAY_INIT2(buffer_vld2, float, 32, 2);
  PAD(buffer_vld2_pad, float, 32, 2);

  VECT_ARRAY_INIT2(buffer_vld2, int, 8, 16);
  PAD(buffer_vld2_pad, int, 8, 16);
  VECT_ARRAY_INIT2(buffer_vld2, int, 16, 8);
  PAD(buffer_vld2_pad, int, 16, 8);
  VECT_ARRAY_INIT2(buffer_vld2, int, 32, 4);
  PAD(buffer_vld2_pad, int, 32, 4);
  VECT_ARRAY_INIT2(buffer_vld2, int, 64, 2);
  PAD(buffer_vld2_pad, int, 64, 2);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 8, 16);
  PAD(buffer_vld2_pad, uint, 8, 16);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 16, 8);
  PAD(buffer_vld2_pad, uint, 16, 8);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 32, 4);
  PAD(buffer_vld2_pad, uint, 32, 4);
  VECT_ARRAY_INIT2(buffer_vld2, uint, 64, 2);
  PAD(buffer_vld2_pad, uint, 64, 2);
  VECT_ARRAY_INIT2(buffer_vld2, poly, 8, 16);
  PAD(buffer_vld2_pad, poly, 8, 16);
  VECT_ARRAY_INIT2(buffer_vld2, poly, 16, 8);
  PAD(buffer_vld2_pad, poly, 16, 8);
  VECT_ARRAY_INIT2(buffer_vld2, float, 32, 4);
  PAD(buffer_vld2_pad, float, 32, 4);

  /* Input buffers for vld3, 1 of each size */
  VECT_ARRAY_INIT3(buffer_vld3, int, 8, 8);
  PAD(buffer_vld3_pad, int, 8, 8);
  VECT_ARRAY_INIT3(buffer_vld3, int, 16, 4);
  PAD(buffer_vld3_pad, int, 16, 4);
  VECT_ARRAY_INIT3(buffer_vld3, int, 32, 2);
  PAD(buffer_vld3_pad, int, 32, 2);
  VECT_ARRAY_INIT3(buffer_vld3, int, 64, 1);
  PAD(buffer_vld3_pad, int, 64, 1);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 8, 8);
  PAD(buffer_vld3_pad, uint, 8, 8);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 16, 4);
  PAD(buffer_vld3_pad, uint, 16, 4);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 32, 2);
  PAD(buffer_vld3_pad, uint, 32, 2);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 64, 1);
  PAD(buffer_vld3_pad, uint, 64, 1);
  VECT_ARRAY_INIT3(buffer_vld3, poly, 8, 8);
  PAD(buffer_vld3_pad, poly, 8, 8);
  VECT_ARRAY_INIT3(buffer_vld3, poly, 16, 4);
  PAD(buffer_vld3_pad, poly, 16, 4);
  VECT_ARRAY_INIT3(buffer_vld3, float, 32, 2);
  PAD(buffer_vld3_pad, float, 32, 2);

  VECT_ARRAY_INIT3(buffer_vld3, int, 8, 16);
  PAD(buffer_vld3_pad, int, 8, 16);
  VECT_ARRAY_INIT3(buffer_vld3, int, 16, 8);
  PAD(buffer_vld3_pad, int, 16, 8);
  VECT_ARRAY_INIT3(buffer_vld3, int, 32, 4);
  PAD(buffer_vld3_pad, int, 32, 4);
  VECT_ARRAY_INIT3(buffer_vld3, int, 64, 2);
  PAD(buffer_vld3_pad, int, 64, 2);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 8, 16);
  PAD(buffer_vld3_pad, uint, 8, 16);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 16, 8);
  PAD(buffer_vld3_pad, uint, 16, 8);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 32, 4);
  PAD(buffer_vld3_pad, uint, 32, 4);
  VECT_ARRAY_INIT3(buffer_vld3, uint, 64, 2);
  PAD(buffer_vld3_pad, uint, 64, 2);
  VECT_ARRAY_INIT3(buffer_vld3, poly, 8, 16);
  PAD(buffer_vld3_pad, poly, 8, 16);
  VECT_ARRAY_INIT3(buffer_vld3, poly, 16, 8);
  PAD(buffer_vld3_pad, poly, 16, 8);
  VECT_ARRAY_INIT3(buffer_vld3, float, 32, 4);
  PAD(buffer_vld3_pad, float, 32, 4);

  /* Input buffers for vld4, 1 of each size */
  VECT_ARRAY_INIT4(buffer_vld4, int, 8, 8);
  PAD(buffer_vld4_pad, int, 8, 8);
  VECT_ARRAY_INIT4(buffer_vld4, int, 16, 4);
  PAD(buffer_vld4_pad, int, 16, 4);
  VECT_ARRAY_INIT4(buffer_vld4, int, 32, 2);
  PAD(buffer_vld4_pad, int, 32, 2);
  VECT_ARRAY_INIT4(buffer_vld4, int, 64, 1);
  PAD(buffer_vld4_pad, int, 64, 1);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 8, 8);
  PAD(buffer_vld4_pad, uint, 8, 8);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 16, 4);
  PAD(buffer_vld4_pad, uint, 16, 4);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 32, 2);
  PAD(buffer_vld4_pad, uint, 32, 2);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 64, 1);
  PAD(buffer_vld4_pad, uint, 64, 1);
  VECT_ARRAY_INIT4(buffer_vld4, poly, 8, 8);
  PAD(buffer_vld4_pad, poly, 8, 8);
  VECT_ARRAY_INIT4(buffer_vld4, poly, 16, 4);
  PAD(buffer_vld4_pad, poly, 16, 4);
  VECT_ARRAY_INIT4(buffer_vld4, float, 32, 2);
  PAD(buffer_vld4_pad, float, 32, 2);

  VECT_ARRAY_INIT4(buffer_vld4, int, 8, 16);
  PAD(buffer_vld4_pad, int, 8, 16);
  VECT_ARRAY_INIT4(buffer_vld4, int, 16, 8);
  PAD(buffer_vld4_pad, int, 16, 8);
  VECT_ARRAY_INIT4(buffer_vld4, int, 32, 4);
  PAD(buffer_vld4_pad, int, 32, 4);
  VECT_ARRAY_INIT4(buffer_vld4, int, 64, 2);
  PAD(buffer_vld4_pad, int, 64, 2);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 8, 16);
  PAD(buffer_vld4_pad, uint, 8, 16);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 16, 8);
  PAD(buffer_vld4_pad, uint, 16, 8);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 32, 4);
  PAD(buffer_vld4_pad, uint, 32, 4);
  VECT_ARRAY_INIT4(buffer_vld4, uint, 64, 2);
  PAD(buffer_vld4_pad, uint, 64, 2);
  VECT_ARRAY_INIT4(buffer_vld4, poly, 8, 16);
  PAD(buffer_vld4_pad, poly, 8, 16);
  VECT_ARRAY_INIT4(buffer_vld4, poly, 16, 8);
  PAD(buffer_vld4_pad, poly, 16, 8);
  VECT_ARRAY_INIT4(buffer_vld4, float, 32, 4);
  PAD(buffer_vld4_pad, float, 32, 4);

  /* Check vld2/vld2q.  */
  clean_results ();
#define TEST_MSG "VLD2/VLD2Q"
  TEST_ALL_VLDX(2);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld2_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(2, 1);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld2_1, "chunk 1");

  /* Check vld3/vld3q.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD3/VLD3Q"
  TEST_ALL_VLDX(3);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld3_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(3, 1);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld3_1, "chunk 1");

  TEST_ALL_EXTRA_CHUNKS(3, 2);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld3_2, "chunk 2");

  /* Check vld4/vld4q.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD4/VLD4Q"
  TEST_ALL_VLDX(4);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld4_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(4, 1);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld4_1, "chunk 1");

  TEST_ALL_EXTRA_CHUNKS(4, 2);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld4_2, "chunk 2");

  TEST_ALL_EXTRA_CHUNKS(4, 3);
  CHECK_RESULTS_NAMED (TEST_MSG, expected_vld4_3, "chunk 3");
}

int main (void)
{
  exec_vldX ();
  return 0;
}
