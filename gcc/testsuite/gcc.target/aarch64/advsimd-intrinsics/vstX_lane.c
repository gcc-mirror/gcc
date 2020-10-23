#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vst2, chunk 0.  */
VECT_VAR_DECL(expected_st2_0,int,8,8) [] = { 0xf0, 0xf1, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,int,16,4) [] = { 0xfff0, 0xfff1, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st2_0,uint,8,8) [] = { 0xf0, 0xf1, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,uint,16,4) [] = { 0xfff0, 0xfff1, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st2_0,poly,8,8) [] = { 0xf0, 0xf1, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_st2_0,int,16,8) [] = { 0xfff0, 0xfff1, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						 0x0, 0x0 };

/* Expected results for vst2, chunk 1.  */
VECT_VAR_DECL(expected_st2_1,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,hfloat,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst3, chunk 0.  */
VECT_VAR_DECL(expected_st3_0,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0 };
VECT_VAR_DECL(expected_st3_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st3_0,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0 };
VECT_VAR_DECL(expected_st3_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st3_0,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0 };
VECT_VAR_DECL(expected_st3_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0x0 };
VECT_VAR_DECL(expected_st3_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_st3_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					      0xfffffff2, 0x0 };
VECT_VAR_DECL(expected_st3_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0x0 };
VECT_VAR_DECL(expected_st3_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						 0xc1600000, 0x0 };

/* Expected results for vst3, chunk 1.  */
VECT_VAR_DECL(expected_st3_1,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,int,32,2) [] = { 0xfffffff2, 0x0 };
VECT_VAR_DECL(expected_st3_1,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,uint,32,2) [] = { 0xfffffff2, 0x0 };
VECT_VAR_DECL(expected_st3_1,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,hfloat,32,2) [] = { 0xc1600000, 0x0 };
VECT_VAR_DECL(expected_st3_1,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst3, chunk 2.  */
VECT_VAR_DECL(expected_st3_2,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,hfloat,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 0.  */
VECT_VAR_DECL(expected_st4_0,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_st4_0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st4_0,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_st4_0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_st4_0,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_st4_0,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected_st4_0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_st4_0,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					      0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_st4_0,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					       0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_st4_0,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_0,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
						 0xc1600000, 0xc1500000 };

/* Expected results for vst4, chunk 1.  */
VECT_VAR_DECL(expected_st4_1,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,int,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_st4_1,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,uint,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_st4_1,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_st4_1,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 2.  */
VECT_VAR_DECL(expected_st4_2,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,hfloat,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 3.  */
VECT_VAR_DECL(expected_st4_3,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,poly,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,poly,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,hfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,hfloat,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,poly,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,hfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,hfloat,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };

/* Declare additional input buffers as needed.  */
/* Input buffers for vld2_lane.  */
VECT_VAR_DECL_INIT(buffer_vld2_lane, int, 8, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, int, 16, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, int, 32, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, int, 64, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, uint, 8, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, uint, 16, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, uint, 32, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, uint, 64, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, poly, 8, 2);
VECT_VAR_DECL_INIT(buffer_vld2_lane, poly, 16, 2);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer_vld2_lane, float, 16, 2);
#endif
VECT_VAR_DECL_INIT(buffer_vld2_lane, float, 32, 2);

/* Input buffers for vld3_lane.  */
VECT_VAR_DECL_INIT(buffer_vld3_lane, int, 8, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, int, 16, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, int, 32, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, int, 64, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, uint, 8, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, uint, 16, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, uint, 32, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, uint, 64, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, poly, 8, 3);
VECT_VAR_DECL_INIT(buffer_vld3_lane, poly, 16, 3);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer_vld3_lane, float, 16, 3);
#endif
VECT_VAR_DECL_INIT(buffer_vld3_lane, float, 32, 3);

/* Input buffers for vld4_lane.  */
VECT_VAR_DECL_INIT(buffer_vld4_lane, int, 8, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, int, 16, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, int, 32, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, int, 64, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, uint, 8, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, uint, 16, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, uint, 32, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, uint, 64, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, poly, 8, 4);
VECT_VAR_DECL_INIT(buffer_vld4_lane, poly, 16, 4);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer_vld4_lane, float, 16, 4);
#endif
VECT_VAR_DECL_INIT(buffer_vld4_lane, float, 32, 4);

void exec_vstX_lane (void)
{
  /* In this case, input variables are arrays of vectors.  */
#define DECL_VSTX_LANE(T1, W, N, X)					\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector, T1, W, N, X);	\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector_src, T1, W, N, X);	\
  VECT_VAR_DECL(result_bis_##X, T1, W, N)[X * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behavior. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
  /* We also use another extra input buffer (buffer_src), which we
     fill with 0xAA, and which it used to load a vector from which we
     read a given lane.  */
#define TEST_VSTX_LANE(Q, T1, T2, W, N, X, L)				 \
  memset (VECT_VAR(buffer_src, T1, W, N), 0xAA,				 \
	  sizeof(VECT_VAR(buffer_src, T1, W, N)));			 \
  memset (VECT_VAR(result_bis_##X, T1, W, N), 0,			 \
	  sizeof(VECT_VAR(result_bis_##X, T1, W, N)));			 \
									 \
  VECT_ARRAY_VAR(vector_src, T1, W, N, X) =				 \
    vld##X##Q##_##T2##W(VECT_VAR(buffer_src, T1, W, N));		 \
									 \
  VECT_ARRAY_VAR(vector, T1, W, N, X) =					 \
    /* Use dedicated init buffer, of size X.  */			 \
    vld##X##Q##_lane_##T2##W(VECT_VAR(buffer_vld##X##_lane, T1, W, X),	 \
			     VECT_ARRAY_VAR(vector_src, T1, W, N, X),	 \
			     L);					 \
  vst##X##Q##_lane_##T2##W(VECT_VAR(result_bis_##X, T1, W, N),		 \
			   VECT_ARRAY_VAR(vector, T1, W, N, X),		 \
			   L);						 \
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[Y].  */
#define TEST_EXTRA_CHUNK(T1, W, N, X, Y)		\
  memcpy(VECT_VAR(result, T1, W, N),			\
	 &(VECT_VAR(result_bis_##X, T1, W, N)[Y*N]),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* We need all variants in 64 bits, but there is no 64x2 variant,
     nor 128 bits vectors of int8/uint8/poly8.  */
#define DECL_ALL_VSTX_LANE_NO_FP16(X)		\
  DECL_VSTX_LANE(int, 8, 8, X);			\
  DECL_VSTX_LANE(int, 16, 4, X);		\
  DECL_VSTX_LANE(int, 32, 2, X);		\
  DECL_VSTX_LANE(uint, 8, 8, X);		\
  DECL_VSTX_LANE(uint, 16, 4, X);		\
  DECL_VSTX_LANE(uint, 32, 2, X);		\
  DECL_VSTX_LANE(poly, 8, 8, X);		\
  DECL_VSTX_LANE(poly, 16, 4, X);		\
  DECL_VSTX_LANE(float, 32, 2, X);		\
  DECL_VSTX_LANE(int, 16, 8, X);		\
  DECL_VSTX_LANE(int, 32, 4, X);		\
  DECL_VSTX_LANE(uint, 16, 8, X);		\
  DECL_VSTX_LANE(uint, 32, 4, X);		\
  DECL_VSTX_LANE(poly, 16, 8, X);		\
  DECL_VSTX_LANE(float, 32, 4, X)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define DECL_ALL_VSTX_LANE(X)		\
  DECL_ALL_VSTX_LANE_NO_FP16(X);	\
  DECL_VSTX_LANE(float, 16, 4, X);	\
  DECL_VSTX_LANE(float, 16, 8, X)
#else
#define DECL_ALL_VSTX_LANE(X) DECL_ALL_VSTX_LANE_NO_FP16(X)
#endif

#define DUMMY_ARRAY(V, T, W, N, L) VECT_VAR_DECL(V,T,W,N)[N*L]

  /* Use the same lanes regardless of the size of the array (X), for
     simplicity.  */
#define TEST_ALL_VSTX_LANE_NO_FP16(X)		\
  TEST_VSTX_LANE(, int, s, 8, 8, X, 7);		\
  TEST_VSTX_LANE(, int, s, 16, 4, X, 2);	\
  TEST_VSTX_LANE(, int, s, 32, 2, X, 0);	\
  TEST_VSTX_LANE(, float, f, 32, 2, X, 0);	\
  TEST_VSTX_LANE(, uint, u, 8, 8, X, 4);	\
  TEST_VSTX_LANE(, uint, u, 16, 4, X, 3);	\
  TEST_VSTX_LANE(, uint, u, 32, 2, X, 1);	\
  TEST_VSTX_LANE(, poly, p, 8, 8, X, 4);	\
  TEST_VSTX_LANE(, poly, p, 16, 4, X, 3);	\
  TEST_VSTX_LANE(q, int, s, 16, 8, X, 6);	\
  TEST_VSTX_LANE(q, int, s, 32, 4, X, 2);	\
  TEST_VSTX_LANE(q, uint, u, 16, 8, X, 5);	\
  TEST_VSTX_LANE(q, uint, u, 32, 4, X, 0);	\
  TEST_VSTX_LANE(q, poly, p, 16, 8, X, 5);	\
  TEST_VSTX_LANE(q, float, f, 32, 4, X, 2)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define TEST_ALL_VSTX_LANE(X)			\
  TEST_ALL_VSTX_LANE_NO_FP16(X);		\
  TEST_VSTX_LANE(, float, f, 16, 4, X, 2);	\
  TEST_VSTX_LANE(q, float, f, 16, 8, X, 6)
#else
#define TEST_ALL_VSTX_LANE(X) TEST_ALL_VSTX_LANE_NO_FP16(X)
#endif

#define TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y)	\
  TEST_EXTRA_CHUNK(int, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(int, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(int, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 8, 8, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(float, 32, 2, X, Y);		\
  TEST_EXTRA_CHUNK(int, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(int, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(uint, 32, 4, X, Y);		\
  TEST_EXTRA_CHUNK(poly, 16, 8, X, Y);		\
  TEST_EXTRA_CHUNK(float, 32, 4, X, Y)

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
#define TEST_ALL_EXTRA_CHUNKS(X,Y)		\
  TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y);		\
  TEST_EXTRA_CHUNK(float, 16, 4, X, Y);		\
  TEST_EXTRA_CHUNK(float, 16, 8, X, Y)
#else
#define TEST_ALL_EXTRA_CHUNKS(X,Y) TEST_ALL_EXTRA_CHUNKS_NO_FP16(X, Y)
#endif

  /* Declare the temporary buffers / variables.  */
  DECL_ALL_VSTX_LANE(2);
  DECL_ALL_VSTX_LANE(3);
  DECL_ALL_VSTX_LANE(4);

  /* Define dummy input arrays, large enough for x4 vectors.  */
  DUMMY_ARRAY(buffer_src, int, 8, 8, 4);
  DUMMY_ARRAY(buffer_src, int, 16, 4, 4);
  DUMMY_ARRAY(buffer_src, int, 32, 2, 4);
  DUMMY_ARRAY(buffer_src, uint, 8, 8, 4);
  DUMMY_ARRAY(buffer_src, uint, 16, 4, 4);
  DUMMY_ARRAY(buffer_src, uint, 32, 2, 4);
  DUMMY_ARRAY(buffer_src, poly, 8, 8, 4);
  DUMMY_ARRAY(buffer_src, poly, 16, 4, 4);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  DUMMY_ARRAY(buffer_src, float, 16, 4, 4);
#endif
  DUMMY_ARRAY(buffer_src, float, 32, 2, 4);
  DUMMY_ARRAY(buffer_src, int, 16, 8, 4);
  DUMMY_ARRAY(buffer_src, int, 32, 4, 4);
  DUMMY_ARRAY(buffer_src, uint, 16, 8, 4);
  DUMMY_ARRAY(buffer_src, uint, 32, 4, 4);
  DUMMY_ARRAY(buffer_src, poly, 16, 8, 4);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  DUMMY_ARRAY(buffer_src, float, 16, 8, 4);
#endif
  DUMMY_ARRAY(buffer_src, float, 32, 4, 4);

  /* Check vst2_lane/vst2q_lane.  */
  clean_results ();
#define TEST_MSG "VST2_LANE/VST2Q_LANE"
  TEST_ALL_VSTX_LANE(2);

#define CMT " (chunk 0)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st2_0, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st2_0, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st2_0, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st2_0, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st2_0, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st2_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st2_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st2_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st2_0, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st2_0, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st2_0, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st2_0, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st2_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st2_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st2_0, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st2_0, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st2_0, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(2, 1);
#undef CMT
#define CMT " (chunk 1)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st2_1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st2_1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st2_1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st2_1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st2_1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st2_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st2_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st2_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st2_1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st2_1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st2_1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st2_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st2_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st2_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st2_1, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st2_1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st2_1, CMT);
#endif


  /* Check vst3_lane/vst3q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST3_LANE/VST3Q_LANE"
  TEST_ALL_VSTX_LANE(3);

#undef CMT
#define CMT " (chunk 0)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st3_0, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st3_0, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st3_0, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st3_0, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st3_0, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st3_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st3_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st3_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st3_0, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st3_0, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st3_0, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st3_0, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st3_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st3_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st3_0, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st3_0, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st3_0, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(3, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st3_1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st3_1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st3_1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st3_1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st3_1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st3_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st3_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st3_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st3_1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st3_1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st3_1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st3_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st3_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st3_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st3_1, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st3_1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st3_1, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(3, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st3_2, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st3_2, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st3_2, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st3_2, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st3_2, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st3_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st3_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st3_2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st3_2, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st3_2, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st3_2, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st3_2, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st3_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st3_2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st3_2, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st3_2, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st3_2, CMT);
#endif


  /* Check vst4_lane/vst4q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST4_LANE/VST4Q_LANE"
  TEST_ALL_VSTX_LANE(4);

#undef CMT
#define CMT " (chunk 0)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st4_0, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st4_0, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st4_0, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st4_0, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st4_0, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st4_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st4_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st4_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st4_0, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st4_0, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st4_0, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st4_0, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st4_0, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st4_0, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st4_0, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st4_0, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st4_0, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(4, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st4_1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st4_1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st4_1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st4_1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st4_1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st4_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st4_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st4_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st4_1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st4_1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st4_1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st4_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st4_1, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st4_1, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st4_1, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st4_1, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st4_1, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(4, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st4_2, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st4_2, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st4_2, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st4_2, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st4_2, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st4_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st4_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st4_2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st4_2, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st4_2, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st4_2, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st4_2, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st4_2, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st4_2, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st4_2, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st4_2, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st4_2, CMT);
#endif

  TEST_ALL_EXTRA_CHUNKS(4, 3);

#undef CMT
#define CMT " (chunk 3)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_st4_3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_st4_3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_st4_3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_st4_3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_st4_3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_st4_3, CMT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_st4_3, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_st4_3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_st4_3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_st4_3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_st4_3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_st4_3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_st4_3, CMT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_st4_3, CMT);
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_st4_3, CMT);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_st4_3, CMT);
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_st4_3, CMT);
#endif
}

int main (void)
{
  exec_vstX_lane ();
  return 0;
}
