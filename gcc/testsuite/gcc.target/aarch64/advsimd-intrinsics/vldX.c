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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld2_0,hmfloat,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7 };
#endif
VECT_VAR_DECL(expected_vld2_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected_vld2_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld2_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld2_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld2_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld2_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld2_0,hmfloat,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						   0xf4, 0xf5, 0xf6, 0xf7,
						   0xf8, 0xf9, 0xfa, 0xfb,
						   0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld2_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
						  0xca00, 0xc980, 0xc900, 0xc880 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld2_1,hmfloat,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
						  0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld2_1,hfloat,16,4) [] = { 0xca00, 0xc980, 0xc900, 0xc880 };
VECT_VAR_DECL(expected_vld2_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld2_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld2_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld2_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld2_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld2_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld2_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld2_1,hmfloat,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						   0x4, 0x5, 0x6, 0x7,
						   0x8, 0x9, 0xa, 0xb,
						   0xc, 0xd, 0xe, 0xf };
#endif
VECT_VAR_DECL(expected_vld2_1,hfloat,16,8) [] = { 0xc800, 0xc700, 0xc600, 0xc500,
						  0xc400, 0xc200, 0xc000, 0xbc00 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_0,hmfloat,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7 };
#endif
VECT_VAR_DECL(expected_vld3_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected_vld3_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld3_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld3_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld3_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld3_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_0,hmfloat,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						   0xf4, 0xf5, 0xf6, 0xf7,
						   0xf8, 0xf9, 0xfa, 0xfb,
						   0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld3_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
						  0xca00, 0xc980, 0xc900, 0xc880 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_1,hmfloat,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
						  0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld3_1,hfloat,16,4) [] = { 0xca00, 0xc980, 0xc900, 0xc880 };
VECT_VAR_DECL(expected_vld3_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld3_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld3_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld3_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld3_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld3_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld3_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_1,hmfloat,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						   0x4, 0x5, 0x6, 0x7,
						   0x8, 0x9, 0xa, 0xb,
						   0xc, 0xd, 0xe, 0xf };
#endif
VECT_VAR_DECL(expected_vld3_1,hfloat,16,8) [] = { 0xc800, 0xc700, 0xc600, 0xc500,
						  0xc400, 0xc200, 0xc000, 0xbc00 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_2,hmfloat,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
						  0x4, 0x5, 0x6, 0x7 };
#endif
VECT_VAR_DECL(expected_vld3_2,hfloat,16,4) [] = { 0xc800, 0xc700, 0xc600, 0xc500 };
VECT_VAR_DECL(expected_vld3_2,hfloat,32,2) [] = { 0xc1400000, 0xc1300000 };
VECT_VAR_DECL(expected_vld3_2,int,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
					       0x14, 0x15, 0x16, 0x17,
					       0x18, 0x19, 0x1a, 0x1b,
					       0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,int,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,int,32,4) [] = { 0xfffffff8, 0xfffffff9,
					       0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld3_2,uint,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,uint,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld3_2,uint,32,4) [] = { 0xfffffff8, 0xfffffff9,
						0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld3_2,poly,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld3_2,poly,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld3_2,hmfloat,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						   0x14, 0x15, 0x16, 0x17,
						   0x18, 0x19, 0x1a, 0x1b,
						   0x1c, 0x1d, 0x1e, 0x1f };
#endif
VECT_VAR_DECL(expected_vld3_2,hfloat,16,8) [] = { 0x0000, 0x3c00, 0x4000, 0x4200,
						  0x4400, 0x4500, 0x4600, 0x4700 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_0,hmfloat,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7 };
#endif
VECT_VAR_DECL(expected_vld4_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_vld4_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected_vld4_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_vld4_0,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_0,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_vld4_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_vld4_0,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_vld4_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						0xfff4, 0xfff5, 0xfff6, 0xfff7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_0,hmfloat,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						   0xf4, 0xf5, 0xf6, 0xf7,
						   0xf8, 0xf9, 0xfa, 0xfb,
						   0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld4_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
						  0xca00, 0xc980, 0xc900, 0xc880 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_1,hmfloat,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
						  0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected_vld4_1,hfloat,16,4) [] = { 0xca00, 0xc980, 0xc900, 0xc880 };
VECT_VAR_DECL(expected_vld4_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_vld4_1,int,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7,
					       0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,int,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
					       0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_1,int,32,4) [] = { 0xfffffff4, 0xfffffff5,
					       0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_1,uint,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,uint,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected_vld4_1,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
						0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected_vld4_1,poly,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7,
						0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_1,poly,16,8) [] = { 0xfff8, 0xfff9, 0xfffa, 0xfffb,
						0xfffc, 0xfffd, 0xfffe, 0xffff };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_1,hmfloat,8,16) [] = { 0x0, 0x1, 0x2, 0x3,
						   0x4, 0x5, 0x6, 0x7,
						   0x8, 0x9, 0xa, 0xb,
						   0xc, 0xd, 0xe, 0xf };
#endif
VECT_VAR_DECL(expected_vld4_1,hfloat,16,8) [] = { 0xc800, 0xc700, 0xc600, 0xc500,
						  0xc400, 0xc200, 0xc000, 0xbc00 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_2,hmfloat,8,8) [] = { 0x0, 0x1, 0x2, 0x3,
						  0x4, 0x5, 0x6, 0x7 };
#endif
VECT_VAR_DECL(expected_vld4_2,hfloat,16,4) [] = { 0xc800, 0xc700, 0xc600, 0xc500 };
VECT_VAR_DECL(expected_vld4_2,hfloat,32,2) [] = { 0xc1400000, 0xc1300000 };
VECT_VAR_DECL(expected_vld4_2,int,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
					       0x14, 0x15, 0x16, 0x17,
					       0x18, 0x19, 0x1a, 0x1b,
					       0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,int,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
					       0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,int,32,4) [] = { 0xfffffff8, 0xfffffff9,
					       0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld4_2,uint,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,uint,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected_vld4_2,uint,32,4) [] = { 0xfffffff8, 0xfffffff9,
						0xfffffffa, 0xfffffffb };
VECT_VAR_DECL(expected_vld4_2,poly,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						0x14, 0x15, 0x16, 0x17,
						0x18, 0x19, 0x1a, 0x1b,
						0x1c, 0x1d, 0x1e, 0x1f };
VECT_VAR_DECL(expected_vld4_2,poly,16,8) [] = { 0x0, 0x1, 0x2, 0x3,
						0x4, 0x5, 0x6, 0x7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_2,hmfloat,8,16) [] = { 0x10, 0x11, 0x12, 0x13,
						   0x14, 0x15, 0x16, 0x17,
						   0x18, 0x19, 0x1a, 0x1b,
						   0x1c, 0x1d, 0x1e, 0x1f };
#endif
VECT_VAR_DECL(expected_vld4_2,hfloat,16,8) [] = { 0x0000, 0x3c00, 0x4000, 0x4200,
						  0x4400, 0x4500, 0x4600, 0x4700 };
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
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_3,hmfloat,8,8) [] = { 0x8, 0x9, 0xa, 0xb,
						  0xc, 0xd, 0xe, 0xf };
#endif
VECT_VAR_DECL(expected_vld4_3,hfloat,16,4) [] = { 0xc400, 0xc200, 0xc000, 0xbc00 };
VECT_VAR_DECL(expected_vld4_3,hfloat,32,2) [] = { 0xc1200000, 0xc1100000 };
VECT_VAR_DECL(expected_vld4_3,int,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
					       0x24, 0x25, 0x26, 0x27,
					       0x28, 0x29, 0x2a, 0x2b,
					       0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,int,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
					       0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,int,32,4) [] = { 0xfffffffc, 0xfffffffd,
					       0xfffffffe, 0xffffffff };
VECT_VAR_DECL(expected_vld4_3,uint,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
						0x24, 0x25, 0x26, 0x27,
						0x28, 0x29, 0x2a, 0x2b,
						0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,uint,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
VECT_VAR_DECL(expected_vld4_3,uint,32,4) [] = { 0xfffffffc, 0xfffffffd,
						0xfffffffe, 0xffffffff };
VECT_VAR_DECL(expected_vld4_3,poly,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
						0x24, 0x25, 0x26, 0x27,
						0x28, 0x29, 0x2a, 0x2b,
						0x2c, 0x2d, 0x2e, 0x2f };
VECT_VAR_DECL(expected_vld4_3,poly,16,8) [] = { 0x8, 0x9, 0xa, 0xb,
						0xc, 0xd, 0xe, 0xf };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected_vld4_3,hmfloat,8,16) [] = { 0x20, 0x21, 0x22, 0x23,
						   0x24, 0x25, 0x26, 0x27,
						   0x28, 0x29, 0x2a, 0x2b,
						   0x2c, 0x2d, 0x2e, 0x2f };
#endif
VECT_VAR_DECL(expected_vld4_3,hfloat,16,8) [] = { 0x4800, 0x4880, 0x4900, 0x4980,
						  0x4a00, 0x4a80, 0x4b00, 0x4b80 };
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
     subset which is used to check the actual behavior. The next
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
#define DECL_ALL_VLDX_NO_FP16(X)		\
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
  MFLOAT8_ONLY(DECL_VLDX(mfloat, 8, 8, X));	\
  DECL_VLDX(float, 32, 2, X);			\
  DECL_VLDX(int, 8, 16, X);			\
  DECL_VLDX(int, 16, 8, X);			\
  DECL_VLDX(int, 32, 4, X);			\
  DECL_VLDX(uint, 8, 16, X);			\
  DECL_VLDX(uint, 16, 8, X);			\
  DECL_VLDX(uint, 32, 4, X);			\
  DECL_VLDX(poly, 8, 16, X);			\
  DECL_VLDX(poly, 16, 8, X);			\
  MFLOAT8_ONLY(DECL_VLDX(mfloat, 8, 16, X));	\
  DECL_VLDX(float, 32, 4, X)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define DECL_ALL_VLDX(X)	\
  DECL_ALL_VLDX_NO_FP16(X);	\
  DECL_VLDX(float, 16, 4, X);	\
  DECL_VLDX(float, 16, 8, X)
#else
#define DECL_ALL_VLDX(X) DECL_ALL_VLDX_NO_FP16(X)
#endif

#define TEST_ALL_VLDX_NO_FP16(X)		\
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
  MFLOAT8_ONLY(TEST_VLDX(, mfloat, mf, 8, 8, X)); \
  TEST_VLDX(, float, f, 32, 2, X);		\
  TEST_VLDX(q, int, s, 8, 16, X);		\
  TEST_VLDX(q, int, s, 16, 8, X);		\
  TEST_VLDX(q, int, s, 32, 4, X);		\
  TEST_VLDX(q, uint, u, 8, 16, X);		\
  TEST_VLDX(q, uint, u, 16, 8, X);		\
  TEST_VLDX(q, uint, u, 32, 4, X);		\
  TEST_VLDX(q, poly, p, 8, 16, X);		\
  TEST_VLDX(q, poly, p, 16, 8, X);		\
  MFLOAT8_ONLY(TEST_VLDX(q, mfloat, mf, 8, 16, X)); \
  TEST_VLDX(q, float, f, 32, 4, X)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define TEST_ALL_VLDX(X)		\
  TEST_ALL_VLDX_NO_FP16(X);		\
  TEST_VLDX(, float, f, 16, 4, X);	\
  TEST_VLDX(q, float, f, 16, 8, X)
#else
#define TEST_ALL_VLDX(X) TEST_ALL_VLDX_NO_FP16(X)
#endif

#define TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y)	\
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
  MFLOAT8_ONLY(TEST_EXTRA_CHUNK(mfloat, 8, 8, X, Y)); \
  TEST_EXTRA_CHUNK(float, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(int, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(int, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(int, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 8, 16, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 16, 8, X, Y);		\
  MFLOAT8_ONLY(TEST_EXTRA_CHUNK(mfloat, 8, 16, X, Y)); \
  TEST_EXTRA_CHUNK(float, 32, 4, X, Y)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define TEST_ALL_EXTRA_CHUNKS(X, Y)		\
  TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y)		\
  TEST_EXTRA_CHUNK(float, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(float, 16, 8, X, Y);
#else
#define TEST_ALL_EXTRA_CHUNKS(X, Y) TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y)
#endif

  /* vldX supports all vector types except [u]int64x2.  */
#define CHECK_RESULTS_VLDX_NO_FP16(test_name,EXPECTED,comment)		\
    CHECK(test_name, int, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, int, 64, 1, PRIx64, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 64, 1, PRIx64, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 8, PRIx8, EXPECTED, comment);	\
    CHECK_POLY(test_name, poly, 16, 4, PRIx16, EXPECTED, comment);	\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 8, PRIx8, EXPECTED, comment)); \
    CHECK_FP(test_name, float, 32, 2, PRIx32, EXPECTED, comment);	\
									\
    CHECK(test_name, int, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 16, PRIx8, EXPECTED, comment);	\
    CHECK_POLY(test_name, poly, 16, 8, PRIx16, EXPECTED, comment);	\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 16, PRIx8, EXPECTED, comment)); \
    CHECK_FP(test_name, float, 32, 4, PRIx32, EXPECTED, comment)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define CHECK_RESULTS_VLDX(test_name,EXPECTED,comment)			\
  {									\
    CHECK_RESULTS_VLDX_NO_FP16(test_name, EXPECTED, comment);		\
    CHECK_FP(test_name, float, 16, 4, PRIx16, EXPECTED, comment);	\
    CHECK_FP(test_name, float, 16, 8, PRIx16, EXPECTED, comment);	\
  }
#else
#define CHECK_RESULTS_VLDX(test_name, EXPECTED, comment)		\
  { CHECK_RESULTS_VLDX_NO_FP16(test_name, EXPECTED, comment); }
#endif

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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld2, mfloat, 8, 8, 2);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld2, mfloat, 8, 8, 2),
		    VECT_ARRAY_VAR(buffer_vld2, int, 8, 8, 2), 8 * 2);
  PAD(buffer_vld2_pad, mfloat, 8, 8);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT2(buffer_vld2, float, 16, 4);
  PAD(buffer_vld2_pad, float, 16, 4);
#endif
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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld2, mfloat, 8, 16, 2);
  PAD(buffer_vld2_pad, mfloat, 8, 16);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld2, mfloat, 8, 16, 2),
		    VECT_ARRAY_VAR(buffer_vld2, int, 8, 16, 2), 16 * 2);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT2(buffer_vld2, float, 16, 8);
  PAD(buffer_vld2_pad, float, 16, 8);
#endif
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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld3, mfloat, 8, 8, 3);
  PAD(buffer_vld3_pad, mfloat, 8, 8);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld3, mfloat, 8, 8, 3),
		    VECT_ARRAY_VAR(buffer_vld3, int, 8, 8, 3), 8 * 3);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT3(buffer_vld3, float, 16, 4);
  PAD(buffer_vld3_pad, float, 16, 4);
#endif
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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld3, mfloat, 8, 16, 3);
  PAD(buffer_vld3_pad, mfloat, 8, 16);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld3, mfloat, 8, 16, 3),
		    VECT_ARRAY_VAR(buffer_vld3, int, 8, 16, 3), 16 * 3);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT3(buffer_vld3, float, 16, 8);
  PAD(buffer_vld3_pad, float, 16, 8);
#endif
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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld4, mfloat, 8, 8, 4);
  PAD(buffer_vld4_pad, mfloat, 8, 8);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld4, mfloat, 8, 8, 4),
		    VECT_ARRAY_VAR(buffer_vld4, int, 8, 8, 4), 8 * 4);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT4(buffer_vld4, float, 16, 4);
  PAD(buffer_vld4_pad, float, 16, 4);
#endif
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
#if MFLOAT8_SUPPORTED
  VECT_ARRAY(buffer_vld4, mfloat, 8, 16, 4);
  PAD(buffer_vld4_pad, mfloat, 8, 16);
  __builtin_memcpy (VECT_ARRAY_VAR(buffer_vld4, mfloat, 8, 16, 4),
		    VECT_ARRAY_VAR(buffer_vld4, int, 8, 16, 4), 16 * 4);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_ARRAY_INIT4(buffer_vld4, float, 16, 8);
  PAD(buffer_vld4_pad, float, 16, 8);
#endif
  VECT_ARRAY_INIT4(buffer_vld4, float, 32, 4);
  PAD(buffer_vld4_pad, float, 32, 4);

  /* Check vld2/vld2q.  */
  clean_results ();
#define TEST_MSG "VLD2/VLD2Q"
  TEST_ALL_VLDX(2);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld2_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(2, 1);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld2_1, "chunk 1");

  /* Check vld3/vld3q.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD3/VLD3Q"
  TEST_ALL_VLDX(3);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld3_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(3, 1);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld3_1, "chunk 1");

  TEST_ALL_EXTRA_CHUNKS(3, 2);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld3_2, "chunk 2");

  /* Check vld4/vld4q.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD4/VLD4Q"
  TEST_ALL_VLDX(4);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld4_0, "chunk 0");

  TEST_ALL_EXTRA_CHUNKS(4, 1);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld4_1, "chunk 1");

  TEST_ALL_EXTRA_CHUNKS(4, 2);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld4_2, "chunk 2");

  TEST_ALL_EXTRA_CHUNKS(4, 3);
  CHECK_RESULTS_VLDX (TEST_MSG, expected_vld4_3, "chunk 3");
}

int main (void)
{
  exec_vldX ();
  return 0;
}
