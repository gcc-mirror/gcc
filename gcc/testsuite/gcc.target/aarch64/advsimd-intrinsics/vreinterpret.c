#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vreinterpret_s8_xx.  */
VECT_VAR_DECL(expected_s8_1,int,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					    0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_s8_2,int,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					    0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_s8_3,int,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_s8_4,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					    0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_s8_5,int,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					    0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_s8_6,int,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					    0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_s8_7,int,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_s8_8,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					    0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_s8_9,int,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					    0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_s8_10,int,8,8) [] = { 0x00, 0xcc, 0x80, 0xcb,
					     0x00, 0xcb, 0x80, 0xca };

/* Expected results for vreinterpret_s16_xx.  */
VECT_VAR_DECL(expected_s16_1,int,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_s16_2,int,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_s16_3,int,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_s16_4,int,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_s16_5,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_s16_6,int,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_s16_7,int,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_s16_8,int,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_s16_9,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_s16_10,int,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };

/* Expected results for vreinterpret_s32_xx.  */
VECT_VAR_DECL(expected_s32_1,int,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_s32_2,int,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_s32_3,int,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_s32_4,int,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_s32_5,int,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_s32_6,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_s32_7,int,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_s32_8,int,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_s32_9,int,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_s32_10,int,32,2) [] = { 0xcb80cc00, 0xca80cb00 };

/* Expected results for vreinterpret_s64_xx.  */
VECT_VAR_DECL(expected_s64_1,int,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_s64_2,int,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_s64_3,int,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(expected_s64_4,int,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_s64_5,int,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_s64_6,int,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(expected_s64_7,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_s64_8,int,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_s64_9,int,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_s64_10,int,64,1) [] = { 0xca80cb00cb80cc00 };

/* Expected results for vreinterpret_u8_xx.  */
VECT_VAR_DECL(expected_u8_1,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					     0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_u8_2,uint,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_u8_3,uint,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_u8_4,uint,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_u8_5,uint,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_u8_6,uint,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_u8_7,uint,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_u8_8,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					     0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_u8_9,uint,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_u8_10,uint,8,8) [] = { 0x00, 0xcc, 0x80, 0xcb,
					      0x00, 0xcb, 0x80, 0xca };

/* Expected results for vreinterpret_u16_xx.  */
VECT_VAR_DECL(expected_u16_1,uint,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_u16_2,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_u16_3,uint,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_u16_4,uint,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_u16_5,uint,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_u16_6,uint,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_u16_7,uint,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_u16_8,uint,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_u16_9,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_u16_10,uint,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };

/* Expected results for vreinterpret_u32_xx.  */
VECT_VAR_DECL(expected_u32_1,uint,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_u32_2,uint,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_u32_3,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_u32_4,uint,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_u32_5,uint,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_u32_6,uint,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_u32_7,uint,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_u32_8,uint,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_u32_9,uint,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_u32_10,uint,32,2) [] = { 0xcb80cc00, 0xca80cb00 };

/* Expected results for vreinterpret_u64_xx.  */
VECT_VAR_DECL(expected_u64_1,uint,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_u64_2,uint,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_u64_3,uint,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(expected_u64_4,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_u64_5,uint,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_u64_6,uint,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_u64_7,uint,64,1) [] = { 0xfffffff1fffffff0 };
VECT_VAR_DECL(expected_u64_8,uint,64,1) [] = { 0xf7f6f5f4f3f2f1f0 };
VECT_VAR_DECL(expected_u64_9,uint,64,1) [] = { 0xfff3fff2fff1fff0 };
VECT_VAR_DECL(expected_u64_10,uint,64,1) [] = { 0xca80cb00cb80cc00 };

/* Expected results for vreinterpret_p8_xx.  */
VECT_VAR_DECL(expected_p8_1,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					     0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_p8_2,poly,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_p8_3,poly,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_p8_4,poly,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_p8_5,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					     0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_p8_6,poly,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_p8_7,poly,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xf1, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_p8_8,poly,8,8) [] = { 0xf0, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_p8_9,poly,8,8) [] = { 0xf0, 0xff, 0xf1, 0xff,
					     0xf2, 0xff, 0xf3, 0xff };
VECT_VAR_DECL(expected_p8_10,poly,8,8) [] = { 0x00, 0xcc, 0x80, 0xcb,
					      0x00, 0xcb, 0x80, 0xca };

/* Expected results for vreinterpret_p16_xx.  */
VECT_VAR_DECL(expected_p16_1,poly,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_p16_2,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_p16_3,poly,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_p16_4,poly,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_p16_5,poly,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_p16_6,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_p16_7,poly,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_p16_8,poly,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_p16_9,poly,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_p16_10,poly,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };

/* Expected results for vreinterpretq_s8_xx.  */
VECT_VAR_DECL(expected_q_s8_1,int,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
					       0xf2, 0xff, 0xf3, 0xff,
					       0xf4, 0xff, 0xf5, 0xff,
					       0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_s8_2,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
					       0xf1, 0xff, 0xff, 0xff,
					       0xf2, 0xff, 0xff, 0xff,
					       0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_s8_3,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
					       0xff, 0xff, 0xff, 0xff,
					       0xf1, 0xff, 0xff, 0xff,
					       0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_s8_4,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_s8_5,int,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
					       0xf2, 0xff, 0xf3, 0xff,
					       0xf4, 0xff, 0xf5, 0xff,
					       0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_s8_6,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
					       0xf1, 0xff, 0xff, 0xff,
					       0xf2, 0xff, 0xff, 0xff,
					       0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_s8_7,int,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
					       0xff, 0xff, 0xff, 0xff,
					       0xf1, 0xff, 0xff, 0xff,
					       0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_s8_8,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					       0xf4, 0xf5, 0xf6, 0xf7,
					       0xf8, 0xf9, 0xfa, 0xfb,
					       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_s8_9,int,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
					       0xf2, 0xff, 0xf3, 0xff,
					       0xf4, 0xff, 0xf5, 0xff,
					       0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_s8_10,int,8,16) [] = { 0x00, 0xcc, 0x80, 0xcb,
						0x00, 0xcb, 0x80, 0xca,
						0x00, 0xca, 0x80, 0xc9,
						0x00, 0xc9, 0x80, 0xc8 };

/* Expected results for vreinterpretq_s16_xx.  */
VECT_VAR_DECL(expected_q_s16_1,int,16,8) [] = { 0xf1f0, 0xf3f2,
						0xf5f4, 0xf7f6,
						0xf9f8, 0xfbfa,
						0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_s16_2,int,16,8) [] = { 0xfff0, 0xffff,
						0xfff1, 0xffff,
						0xfff2, 0xffff,
						0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_s16_3,int,16,8) [] = { 0xfff0, 0xffff,
						0xffff, 0xffff,
						0xfff1, 0xffff,
						0xffff, 0xffff };
VECT_VAR_DECL(expected_q_s16_4,int,16,8) [] = { 0xf1f0, 0xf3f2,
						0xf5f4, 0xf7f6,
						0xf9f8, 0xfbfa,
						0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_s16_5,int,16,8) [] = { 0xfff0, 0xfff1,
						0xfff2, 0xfff3,
						0xfff4, 0xfff5,
						0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_s16_6,int,16,8) [] = { 0xfff0, 0xffff,
						0xfff1, 0xffff,
						0xfff2, 0xffff,
						0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_s16_7,int,16,8) [] = { 0xfff0, 0xffff,
						0xffff, 0xffff,
						0xfff1, 0xffff,
						0xffff, 0xffff };
VECT_VAR_DECL(expected_q_s16_8,int,16,8) [] = { 0xf1f0, 0xf3f2,
						0xf5f4, 0xf7f6,
						0xf9f8, 0xfbfa,
						0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_s16_9,int,16,8) [] = { 0xfff0, 0xfff1,
						0xfff2, 0xfff3,
						0xfff4, 0xfff5,
						0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_s16_10,int,16,8) [] = { 0xcc00, 0xcb80,
						 0xcb00, 0xca80,
						 0xca00, 0xc980,
						 0xc900, 0xc880 };

/* Expected results for vreinterpretq_s32_xx.  */
VECT_VAR_DECL(expected_q_s32_1,int,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_s32_2,int,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_s32_3,int,32,4) [] = { 0xfffffff0, 0xffffffff,
						0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_s32_4,int,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_s32_5,int,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_s32_6,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
						0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_q_s32_7,int,32,4) [] = { 0xfffffff0, 0xffffffff,
						0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_s32_8,int,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_s32_9,int,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_s32_10,int,32,4) [] = { 0xcb80cc00, 0xca80cb00,
						 0xc980ca00, 0xc880c900 };

/* Expected results for vreinterpretq_s64_xx.  */
VECT_VAR_DECL(expected_q_s64_1,int,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_s64_2,int,64,2) [] = { 0xfff3fff2fff1fff0,
						0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_s64_3,int,64,2) [] = { 0xfffffff1fffffff0,
						0xfffffff3fffffff2 };
VECT_VAR_DECL(expected_q_s64_4,int,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_s64_5,int,64,2) [] = { 0xfff3fff2fff1fff0,
						0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_s64_6,int,64,2) [] = { 0xfffffff1fffffff0,
						0xfffffff3fffffff2 };
VECT_VAR_DECL(expected_q_s64_7,int,64,2) [] = { 0xfffffffffffffff0,
						0xfffffffffffffff1 };
VECT_VAR_DECL(expected_q_s64_8,int,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_s64_9,int,64,2) [] = { 0xfff3fff2fff1fff0,
						0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_s64_10,int,64,2) [] = { 0xca80cb00cb80cc00,
						 0xc880c900c980ca00 };

/* Expected results for vreinterpretq_u8_xx.  */
VECT_VAR_DECL(expected_q_u8_1,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_u8_2,uint,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_u8_3,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xf2, 0xff, 0xff, 0xff,
						0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_u8_4,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_u8_5,uint,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_u8_6,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xf2, 0xff, 0xff, 0xff,
						0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_u8_7,uint,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_u8_8,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_u8_9,uint,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_u8_10,uint,8,16) [] = { 0x00, 0xcc, 0x80, 0xcb,
						 0x00, 0xcb, 0x80, 0xca,
						 0x00, 0xca, 0x80, 0xc9,
						 0x00, 0xc9, 0x80, 0xc8 };

/* Expected results for vreinterpretq_u16_xx.  */
VECT_VAR_DECL(expected_q_u16_1,uint,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_u16_2,uint,16,8) [] = { 0xfff0, 0xfff1,
						 0xfff2, 0xfff3,
						 0xfff4, 0xfff5,
						 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_u16_3,uint,16,8) [] = { 0xfff0, 0xffff,
						 0xfff1, 0xffff,
						 0xfff2, 0xffff,
						 0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_u16_4,uint,16,8) [] = { 0xfff0, 0xffff,
						 0xffff, 0xffff,
						 0xfff1, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_q_u16_5,uint,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_u16_6,uint,16,8) [] = { 0xfff0, 0xffff,
						 0xfff1, 0xffff,
						 0xfff2, 0xffff,
						 0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_u16_7,uint,16,8) [] = { 0xfff0, 0xffff,
						 0xffff, 0xffff,
						 0xfff1, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_q_u16_8,uint,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_u16_9,uint,16,8) [] = { 0xfff0, 0xfff1,
						 0xfff2, 0xfff3,
						 0xfff4, 0xfff5,
						 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_u16_10,uint,16,8) [] = { 0xcc00, 0xcb80,
						  0xcb00, 0xca80,
						  0xca00, 0xc980,
						  0xc900, 0xc880 };

/* Expected results for vreinterpretq_u32_xx.  */
VECT_VAR_DECL(expected_q_u32_1,uint,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						 0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_u32_2,uint,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						 0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_u32_3,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_q_u32_4,uint,32,4) [] = { 0xfffffff0, 0xffffffff,
						 0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_u32_5,uint,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						 0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_u32_6,uint,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						 0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_u32_7,uint,32,4) [] = { 0xfffffff0, 0xffffffff,
						 0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_u32_8,uint,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						 0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_u32_9,uint,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						 0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_u32_10,uint,32,4) [] = { 0xcb80cc00, 0xca80cb00,
						  0xc980ca00, 0xc880c900 };

/* Expected results for vreinterpretq_u64_xx.  */
VECT_VAR_DECL(expected_q_u64_1,uint,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_u64_2,uint,64,2) [] = { 0xfff3fff2fff1fff0,
						0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_u64_3,uint,64,2) [] = { 0xfffffff1fffffff0,
						0xfffffff3fffffff2 };
VECT_VAR_DECL(expected_q_u64_4,uint,64,2) [] = { 0xfffffffffffffff0,
						0xfffffffffffffff1 };
VECT_VAR_DECL(expected_q_u64_5,uint,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_u64_6,uint,64,2) [] = { 0xfff3fff2fff1fff0,
						0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_u64_7,uint,64,2) [] = { 0xfffffff1fffffff0,
						0xfffffff3fffffff2 };
VECT_VAR_DECL(expected_q_u64_8,uint,64,2) [] = { 0xf7f6f5f4f3f2f1f0,
						0xfffefdfcfbfaf9f8 };
VECT_VAR_DECL(expected_q_u64_9,uint,64,2) [] = { 0xfff3fff2fff1fff0,
						 0xfff7fff6fff5fff4 };
VECT_VAR_DECL(expected_q_u64_10,uint,64,2) [] = { 0xca80cb00cb80cc00,
						  0xc880c900c980ca00 };

/* Expected results for vreinterpretq_p8_xx.  */
VECT_VAR_DECL(expected_q_p8_1,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_p8_2,poly,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_p8_3,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xf2, 0xff, 0xff, 0xff,
						0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_p8_4,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_p8_5,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						0xf4, 0xf5, 0xf6, 0xf7,
						0xf8, 0xf9, 0xfa, 0xfb,
						0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_q_p8_6,poly,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_p8_7,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xf2, 0xff, 0xff, 0xff,
						0xf3, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_p8_8,poly,8,16) [] = { 0xf0, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xf1, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_q_p8_9,poly,8,16) [] = { 0xf0, 0xff, 0xf1, 0xff,
						0xf2, 0xff, 0xf3, 0xff,
						0xf4, 0xff, 0xf5, 0xff,
						0xf6, 0xff, 0xf7, 0xff };
VECT_VAR_DECL(expected_q_p8_10,poly,8,16) [] = { 0x00, 0xcc, 0x80, 0xcb,
						 0x00, 0xcb, 0x80, 0xca,
						 0x00, 0xca, 0x80, 0xc9,
						 0x00, 0xc9, 0x80, 0xc8 };

/* Expected results for vreinterpretq_p16_xx.  */
VECT_VAR_DECL(expected_q_p16_1,poly,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_p16_2,poly,16,8) [] = { 0xfff0, 0xfff1,
						 0xfff2, 0xfff3,
						 0xfff4, 0xfff5,
						 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_p16_3,poly,16,8) [] = { 0xfff0, 0xffff,
						 0xfff1, 0xffff,
						 0xfff2, 0xffff,
						 0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_p16_4,poly,16,8) [] = { 0xfff0, 0xffff,
						 0xffff, 0xffff,
						 0xfff1, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_q_p16_5,poly,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_p16_6,poly,16,8) [] = { 0xfff0, 0xfff1,
						 0xfff2, 0xfff3,
						 0xfff4, 0xfff5,
						 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_p16_7,poly,16,8) [] = { 0xfff0, 0xffff,
						 0xfff1, 0xffff,
						 0xfff2, 0xffff,
						 0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_p16_8,poly,16,8) [] = { 0xfff0, 0xffff,
						 0xffff, 0xffff,
						 0xfff1, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_q_p16_9,poly,16,8) [] = { 0xf1f0, 0xf3f2,
						 0xf5f4, 0xf7f6,
						 0xf9f8, 0xfbfa,
						 0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_p16_10,poly,16,8) [] = { 0xcc00, 0xcb80,
						  0xcb00, 0xca80,
						  0xca00, 0xc980,
						  0xc900, 0xc880 };

/* Expected results for vreinterpret_f32_xx.  */
VECT_VAR_DECL(expected_f32_1,hfloat,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_f32_2,hfloat,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_f32_3,hfloat,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_f32_4,hfloat,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_f32_5,hfloat,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_f32_6,hfloat,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_f32_7,hfloat,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_f32_8,hfloat,32,2) [] = { 0xfffffff0, 0xffffffff };
VECT_VAR_DECL(expected_f32_9,hfloat,32,2) [] = { 0xf3f2f1f0, 0xf7f6f5f4 };
VECT_VAR_DECL(expected_f32_10,hfloat,32,2) [] = { 0xfff1fff0, 0xfff3fff2 };
VECT_VAR_DECL(expected_f32_11,hfloat,32,2) [] = { 0xcb80cc00, 0xca80cb00 };

/* Expected results for vreinterpretq_f32_xx.  */
VECT_VAR_DECL(expected_q_f32_1,hfloat,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						   0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_f32_2,hfloat,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						   0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_f32_3,hfloat,32,4) [] = { 0xfffffff0, 0xfffffff1,
						   0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_q_f32_4,hfloat,32,4) [] = { 0xfffffff0, 0xffffffff,
						   0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_f32_5,hfloat,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						   0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_f32_6,hfloat,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						   0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_f32_7,hfloat,32,4) [] = { 0xfffffff0, 0xfffffff1,
						   0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_q_f32_8,hfloat,32,4) [] = { 0xfffffff0, 0xffffffff,
						   0xfffffff1, 0xffffffff };
VECT_VAR_DECL(expected_q_f32_9,hfloat,32,4) [] = { 0xf3f2f1f0, 0xf7f6f5f4,
						   0xfbfaf9f8, 0xfffefdfc };
VECT_VAR_DECL(expected_q_f32_10,hfloat,32,4) [] = { 0xfff1fff0, 0xfff3fff2,
						    0xfff5fff4, 0xfff7fff6 };
VECT_VAR_DECL(expected_q_f32_11,hfloat,32,4) [] = { 0xcb80cc00, 0xca80cb00,
						    0xc980ca00, 0xc880c900 };

/* Expected results for vreinterpret_xx_f32.  */
VECT_VAR_DECL(expected_xx_f32_1,int,8,8) [] = { 0x0, 0x0, 0x80, 0xc1,
						0x0, 0x0, 0x70, 0xc1 };
VECT_VAR_DECL(expected_xx_f32_2,int,16,4) [] = { 0x0, 0xc180, 0x0, 0xc170 };
VECT_VAR_DECL(expected_xx_f32_3,int,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_xx_f32_4,int,64,1) [] = { 0xc1700000c1800000 };
VECT_VAR_DECL(expected_xx_f32_5,uint,8,8) [] = { 0x0, 0x0, 0x80, 0xc1,
						 0x0, 0x0, 0x70, 0xc1 };
VECT_VAR_DECL(expected_xx_f32_6,uint,16,4) [] = { 0x0, 0xc180, 0x0, 0xc170 };
VECT_VAR_DECL(expected_xx_f32_7,uint,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_xx_f32_8,uint,64,1) [] = { 0xc1700000c1800000 };
VECT_VAR_DECL(expected_xx_f32_9,poly,8,8) [] = { 0x0, 0x0, 0x80, 0xc1,
						 0x0, 0x0, 0x70, 0xc1 };
VECT_VAR_DECL(expected_xx_f32_10,poly,16,4) [] = { 0x0, 0xc180, 0x0, 0xc170 };
VECT_VAR_DECL(expected_xx_f32_11,hfloat,16,4) [] = { 0x0, 0xc180, 0x0, 0xc170 };

/* Expected results for vreinterpretq_xx_f32.  */
VECT_VAR_DECL(expected_q_xx_f32_1,int,8,16) [] = { 0x0, 0x0, 0x80, 0xc1,
						   0x0, 0x0, 0x70, 0xc1,
						   0x0, 0x0, 0x60, 0xc1,
						   0x0, 0x0, 0x50, 0xc1 };
VECT_VAR_DECL(expected_q_xx_f32_2,int,16,8) [] = { 0x0, 0xc180, 0x0, 0xc170,
						   0x0, 0xc160, 0x0, 0xc150 };
VECT_VAR_DECL(expected_q_xx_f32_3,int,32,4) [] = { 0xc1800000, 0xc1700000,
						   0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_q_xx_f32_4,int,64,2) [] = { 0xc1700000c1800000,
						   0xc1500000c1600000 };
VECT_VAR_DECL(expected_q_xx_f32_5,uint,8,16) [] = { 0x0, 0x0, 0x80, 0xc1,
						    0x0, 0x0, 0x70, 0xc1,
						    0x0, 0x0, 0x60, 0xc1,
						    0x0, 0x0, 0x50, 0xc1 };
VECT_VAR_DECL(expected_q_xx_f32_6,uint,16,8) [] = { 0x0, 0xc180, 0x0, 0xc170,
						    0x0, 0xc160, 0x0, 0xc150 };
VECT_VAR_DECL(expected_q_xx_f32_7,uint,32,4) [] = { 0xc1800000, 0xc1700000,
						    0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_q_xx_f32_8,uint,64,2) [] = { 0xc1700000c1800000,
						    0xc1500000c1600000 };
VECT_VAR_DECL(expected_q_xx_f32_9,poly,8,16) [] = { 0x0, 0x0, 0x80, 0xc1,
						    0x0, 0x0, 0x70, 0xc1,
						    0x0, 0x0, 0x60, 0xc1,
						    0x0, 0x0, 0x50, 0xc1 };
VECT_VAR_DECL(expected_q_xx_f32_10,poly,16,8) [] = { 0x0, 0xc180, 0x0, 0xc170,
						     0x0, 0xc160, 0x0, 0xc150 };
VECT_VAR_DECL(expected_q_xx_f32_11,hfloat,16,8) [] = { 0x0, 0xc180, 0x0, 0xc170,
						      0x0, 0xc160, 0x0, 0xc150 };

/* Expected results for vreinterpret_f16_xx.  */
VECT_VAR_DECL(expected_f16_1,hfloat,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_f16_2,hfloat,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_f16_3,hfloat,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_f16_4,hfloat,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_f16_5,hfloat,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_f16_6,hfloat,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_f16_7,hfloat,16,4) [] = { 0xfff0, 0xffff, 0xfff1, 0xffff };
VECT_VAR_DECL(expected_f16_8,hfloat,16,4) [] = { 0xfff0, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_f16_9,hfloat,16,4) [] = { 0xf1f0, 0xf3f2, 0xf5f4, 0xf7f6 };
VECT_VAR_DECL(expected_f16_10,hfloat,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };

/* Expected results for vreinterpretq_f16_xx.  */
VECT_VAR_DECL(expected_q_f16_1,hfloat,16,8) [] = { 0xf1f0, 0xf3f2,
						   0xf5f4, 0xf7f6,
						   0xf9f8, 0xfbfa,
						   0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_f16_2,hfloat,16,8) [] = { 0xfff0, 0xfff1,
						   0xfff2, 0xfff3,
						   0xfff4, 0xfff5,
						   0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_f16_3,hfloat,16,8) [] = { 0xfff0, 0xffff,
						   0xfff1, 0xffff,
						   0xfff2, 0xffff,
						   0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_f16_4,hfloat,16,8) [] = { 0xfff0, 0xffff,
						   0xffff, 0xffff,
						   0xfff1, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL(expected_q_f16_5,hfloat,16,8) [] = { 0xf1f0, 0xf3f2,
						   0xf5f4, 0xf7f6,
						   0xf9f8, 0xfbfa,
						   0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_f16_6,hfloat,16,8) [] = { 0xfff0, 0xfff1,
						   0xfff2, 0xfff3,
						   0xfff4, 0xfff5,
						   0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_q_f16_7,hfloat,16,8) [] = { 0xfff0, 0xffff,
						   0xfff1, 0xffff,
						   0xfff2, 0xffff,
						   0xfff3, 0xffff };
VECT_VAR_DECL(expected_q_f16_8,hfloat,16,8) [] = { 0xfff0, 0xffff,
						   0xffff, 0xffff,
						   0xfff1, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL(expected_q_f16_9,hfloat,16,8) [] = { 0xf1f0, 0xf3f2,
						   0xf5f4, 0xf7f6,
						   0xf9f8, 0xfbfa,
						   0xfdfc, 0xfffe };
VECT_VAR_DECL(expected_q_f16_10,hfloat,16,8) [] = { 0xfff0, 0xfff1,
						    0xfff2, 0xfff3,
						    0xfff4, 0xfff5,
						    0xfff6, 0xfff7 };

#define TEST_MSG "VREINTERPRET/VREINTERPRETQ"

void exec_vreinterpret (void)
{
  int i;

  /* Basic test: y=vreinterpret(x), then store the result.  */
#define TEST_VREINTERPRET(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED)	\
  VECT_VAR(vector_res, T1, W, N) =					\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vector, TS1, WS, NS)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

#define TEST_VREINTERPRET_POLY(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vector_res, T1, W, N) =					\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vector, TS1, WS, NS)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

#define TEST_VREINTERPRET_FP(Q, T1, T2, W, N, TS1, TS2, WS, NS, EXPECTED) \
  VECT_VAR(vector_res, T1, W, N) =					\
    vreinterpret##Q##_##T2##W##_##TS2##WS(VECT_VAR(vector, TS1, WS, NS)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK_FP(TEST_MSG, T1, W, N, PRIx##W, EXPECTED, "");

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();


  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* vreinterpret_s8_xx.  */
  TEST_VREINTERPRET(, int, s, 8, 8, int, s, 16, 4, expected_s8_1);
  TEST_VREINTERPRET(, int, s, 8, 8, int, s, 32, 2, expected_s8_2);
  TEST_VREINTERPRET(, int, s, 8, 8, int, s, 64, 1, expected_s8_3);
  TEST_VREINTERPRET(, int, s, 8, 8, uint, u, 8, 8, expected_s8_4);
  TEST_VREINTERPRET(, int, s, 8, 8, uint, u, 16, 4, expected_s8_5);
  TEST_VREINTERPRET(, int, s, 8, 8, uint, u, 32, 2, expected_s8_6);
  TEST_VREINTERPRET(, int, s, 8, 8, uint, u, 64, 1, expected_s8_7);
  TEST_VREINTERPRET(, int, s, 8, 8, poly, p, 8, 8, expected_s8_8);
  TEST_VREINTERPRET(, int, s, 8, 8, poly, p, 16, 4, expected_s8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, int, s, 8, 8, float, f, 16, 4, expected_s8_10);
#endif

  /* vreinterpret_s16_xx.  */
  TEST_VREINTERPRET(, int, s, 16, 4, int, s, 8, 8, expected_s16_1);
  TEST_VREINTERPRET(, int, s, 16, 4, int, s, 32, 2, expected_s16_2);
  TEST_VREINTERPRET(, int, s, 16, 4, int, s, 64, 1, expected_s16_3);
  TEST_VREINTERPRET(, int, s, 16, 4, uint, u, 8, 8, expected_s16_4);
  TEST_VREINTERPRET(, int, s, 16, 4, uint, u, 16, 4, expected_s16_5);
  TEST_VREINTERPRET(, int, s, 16, 4, uint, u, 32, 2, expected_s16_6);
  TEST_VREINTERPRET(, int, s, 16, 4, uint, u, 64, 1, expected_s16_7);
  TEST_VREINTERPRET(, int, s, 16, 4, poly, p, 8, 8, expected_s16_8);
  TEST_VREINTERPRET(, int, s, 16, 4, poly, p, 16, 4, expected_s16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, int, s, 16, 4, float, f, 16, 4, expected_s16_10);
#endif

  /* vreinterpret_s32_xx.  */
  TEST_VREINTERPRET(, int, s, 32, 2, int, s, 8, 8, expected_s32_1);
  TEST_VREINTERPRET(, int, s, 32, 2, int, s, 16, 4, expected_s32_2);
  TEST_VREINTERPRET(, int, s, 32, 2, int, s, 64, 1, expected_s32_3);
  TEST_VREINTERPRET(, int, s, 32, 2, uint, u, 8, 8, expected_s32_4);
  TEST_VREINTERPRET(, int, s, 32, 2, uint, u, 16, 4, expected_s32_5);
  TEST_VREINTERPRET(, int, s, 32, 2, uint, u, 32, 2, expected_s32_6);
  TEST_VREINTERPRET(, int, s, 32, 2, uint, u, 64, 1, expected_s32_7);
  TEST_VREINTERPRET(, int, s, 32, 2, poly, p, 8, 8, expected_s32_8);
  TEST_VREINTERPRET(, int, s, 32, 2, poly, p, 16, 4, expected_s32_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, int, s, 32, 2, float, f, 16, 4, expected_s32_10);
#endif

  /* vreinterpret_s64_xx.  */
  TEST_VREINTERPRET(, int, s, 64, 1, int, s, 8, 8, expected_s64_1);
  TEST_VREINTERPRET(, int, s, 64, 1, int, s, 16, 4, expected_s64_2);
  TEST_VREINTERPRET(, int, s, 64, 1, int, s, 32, 2, expected_s64_3);
  TEST_VREINTERPRET(, int, s, 64, 1, uint, u, 8, 8, expected_s64_4);
  TEST_VREINTERPRET(, int, s, 64, 1, uint, u, 16, 4, expected_s64_5);
  TEST_VREINTERPRET(, int, s, 64, 1, uint, u, 32, 2, expected_s64_6);
  TEST_VREINTERPRET(, int, s, 64, 1, uint, u, 64, 1, expected_s64_7);
  TEST_VREINTERPRET(, int, s, 64, 1, poly, p, 8, 8, expected_s64_8);
  TEST_VREINTERPRET(, int, s, 64, 1, poly, p, 16, 4, expected_s64_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, int, s, 64, 1, float, f, 16, 4, expected_s64_10);
#endif

  /* vreinterpret_u8_xx.  */
  TEST_VREINTERPRET(, uint, u, 8, 8, int, s, 8, 8, expected_u8_1);
  TEST_VREINTERPRET(, uint, u, 8, 8, int, s, 16, 4, expected_u8_2);
  TEST_VREINTERPRET(, uint, u, 8, 8, int, s, 32, 2, expected_u8_3);
  TEST_VREINTERPRET(, uint, u, 8, 8, int, s, 64, 1, expected_u8_4);
  TEST_VREINTERPRET(, uint, u, 8, 8, uint, u, 16, 4, expected_u8_5);
  TEST_VREINTERPRET(, uint, u, 8, 8, uint, u, 32, 2, expected_u8_6);
  TEST_VREINTERPRET(, uint, u, 8, 8, uint, u, 64, 1, expected_u8_7);
  TEST_VREINTERPRET(, uint, u, 8, 8, poly, p, 8, 8, expected_u8_8);
  TEST_VREINTERPRET(, uint, u, 8, 8, poly, p, 16, 4, expected_u8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, uint, u, 8, 8, float, f, 16, 4, expected_u8_10);
#endif

  /* vreinterpret_u16_xx.  */
  TEST_VREINTERPRET(, uint, u, 16, 4, int, s, 8, 8, expected_u16_1);
  TEST_VREINTERPRET(, uint, u, 16, 4, int, s, 16, 4, expected_u16_2);
  TEST_VREINTERPRET(, uint, u, 16, 4, int, s, 32, 2, expected_u16_3);
  TEST_VREINTERPRET(, uint, u, 16, 4, int, s, 64, 1, expected_u16_4);
  TEST_VREINTERPRET(, uint, u, 16, 4, uint, u, 8, 8, expected_u16_5);
  TEST_VREINTERPRET(, uint, u, 16, 4, uint, u, 32, 2, expected_u16_6);
  TEST_VREINTERPRET(, uint, u, 16, 4, uint, u, 64, 1, expected_u16_7);
  TEST_VREINTERPRET(, uint, u, 16, 4, poly, p, 8, 8, expected_u16_8);
  TEST_VREINTERPRET(, uint, u, 16, 4, poly, p, 16, 4, expected_u16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, uint, u, 16, 4, float, f, 16, 4, expected_u16_10);
#endif

  /* vreinterpret_u32_xx.  */
  TEST_VREINTERPRET(, uint, u, 32, 2, int, s, 8, 8, expected_u32_1);
  TEST_VREINTERPRET(, uint, u, 32, 2, int, s, 16, 4, expected_u32_2);
  TEST_VREINTERPRET(, uint, u, 32, 2, int, s, 32, 2, expected_u32_3);
  TEST_VREINTERPRET(, uint, u, 32, 2, int, s, 64, 1, expected_u32_4);
  TEST_VREINTERPRET(, uint, u, 32, 2, uint, u, 8, 8, expected_u32_5);
  TEST_VREINTERPRET(, uint, u, 32, 2, uint, u, 16, 4, expected_u32_6);
  TEST_VREINTERPRET(, uint, u, 32, 2, uint, u, 64, 1, expected_u32_7);
  TEST_VREINTERPRET(, uint, u, 32, 2, poly, p, 8, 8, expected_u32_8);
  TEST_VREINTERPRET(, uint, u, 32, 2, poly, p, 16, 4, expected_u32_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, uint, u, 32, 2, float, f, 16, 4, expected_u32_10);
#endif

  /* vreinterpret_u64_xx.  */
  TEST_VREINTERPRET(, uint, u, 64, 1, int, s, 8, 8, expected_u64_1);
  TEST_VREINTERPRET(, uint, u, 64, 1, int, s, 16, 4, expected_u64_2);
  TEST_VREINTERPRET(, uint, u, 64, 1, int, s, 32, 2, expected_u64_3);
  TEST_VREINTERPRET(, uint, u, 64, 1, int, s, 64, 1, expected_u64_4);
  TEST_VREINTERPRET(, uint, u, 64, 1, uint, u, 8, 8, expected_u64_5);
  TEST_VREINTERPRET(, uint, u, 64, 1, uint, u, 16, 4, expected_u64_6);
  TEST_VREINTERPRET(, uint, u, 64, 1, uint, u, 32, 2, expected_u64_7);
  TEST_VREINTERPRET(, uint, u, 64, 1, poly, p, 8, 8, expected_u64_8);
  TEST_VREINTERPRET(, uint, u, 64, 1, poly, p, 16, 4, expected_u64_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(, uint, u, 64, 1, float, f, 16, 4, expected_u64_10);
#endif

  /* vreinterpret_p8_xx.  */
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, int, s, 8, 8, expected_p8_1);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, int, s, 16, 4, expected_p8_2);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, int, s, 32, 2, expected_p8_3);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, int, s, 64, 1, expected_p8_4);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, uint, u, 8, 8, expected_p8_5);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, uint, u, 16, 4, expected_p8_6);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, uint, u, 32, 2, expected_p8_7);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, uint, u, 64, 1, expected_p8_8);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, poly, p, 16, 4, expected_p8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, float, f, 16, 4, expected_p8_10);
#endif

  /* vreinterpret_p16_xx.  */
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, int, s, 8, 8, expected_p16_1);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, int, s, 16, 4, expected_p16_2);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, int, s, 32, 2, expected_p16_3);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, int, s, 64, 1, expected_p16_4);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, uint, u, 8, 8, expected_p16_5);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, uint, u, 16, 4, expected_p16_6);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, uint, u, 32, 2, expected_p16_7);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, uint, u, 64, 1, expected_p16_8);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, poly, p, 8, 8, expected_p16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, float, f, 16, 4, expected_p16_10);
#endif

  /* vreinterpretq_s8_xx.  */
  TEST_VREINTERPRET(q, int, s, 8, 16, int, s, 16, 8, expected_q_s8_1);
  TEST_VREINTERPRET(q, int, s, 8, 16, int, s, 32, 4, expected_q_s8_2);
  TEST_VREINTERPRET(q, int, s, 8, 16, int, s, 64, 2, expected_q_s8_3);
  TEST_VREINTERPRET(q, int, s, 8, 16, uint, u, 8, 16, expected_q_s8_4);
  TEST_VREINTERPRET(q, int, s, 8, 16, uint, u, 16, 8, expected_q_s8_5);
  TEST_VREINTERPRET(q, int, s, 8, 16, uint, u, 32, 4, expected_q_s8_6);
  TEST_VREINTERPRET(q, int, s, 8, 16, uint, u, 64, 2, expected_q_s8_7);
  TEST_VREINTERPRET(q, int, s, 8, 16, poly, p, 8, 16, expected_q_s8_8);
  TEST_VREINTERPRET(q, int, s, 8, 16, poly, p, 16, 8, expected_q_s8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, int, s, 8, 16, float, f, 16, 8, expected_q_s8_10);
#endif

  /* vreinterpretq_s16_xx.  */
  TEST_VREINTERPRET(q, int, s, 16, 8, int, s, 8, 16, expected_q_s16_1);
  TEST_VREINTERPRET(q, int, s, 16, 8, int, s, 32, 4, expected_q_s16_2);
  TEST_VREINTERPRET(q, int, s, 16, 8, int, s, 64, 2, expected_q_s16_3);
  TEST_VREINTERPRET(q, int, s, 16, 8, uint, u, 8, 16, expected_q_s16_4);
  TEST_VREINTERPRET(q, int, s, 16, 8, uint, u, 16, 8, expected_q_s16_5);
  TEST_VREINTERPRET(q, int, s, 16, 8, uint, u, 32, 4, expected_q_s16_6);
  TEST_VREINTERPRET(q, int, s, 16, 8, uint, u, 64, 2, expected_q_s16_7);
  TEST_VREINTERPRET(q, int, s, 16, 8, poly, p, 8, 16, expected_q_s16_8);
  TEST_VREINTERPRET(q, int, s, 16, 8, poly, p, 16, 8, expected_q_s16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, int, s, 16, 8, float, f, 16, 8, expected_q_s16_10);
#endif

  /* vreinterpretq_s32_xx.  */
  TEST_VREINTERPRET(q, int, s, 32, 4, int, s, 8, 16, expected_q_s32_1);
  TEST_VREINTERPRET(q, int, s, 32, 4, int, s, 16, 8, expected_q_s32_2);
  TEST_VREINTERPRET(q, int, s, 32, 4, int, s, 64, 2, expected_q_s32_3);
  TEST_VREINTERPRET(q, int, s, 32, 4, uint, u, 8, 16, expected_q_s32_4);
  TEST_VREINTERPRET(q, int, s, 32, 4, uint, u, 16, 8, expected_q_s32_5);
  TEST_VREINTERPRET(q, int, s, 32, 4, uint, u, 32, 4, expected_q_s32_6);
  TEST_VREINTERPRET(q, int, s, 32, 4, uint, u, 64, 2, expected_q_s32_7);
  TEST_VREINTERPRET(q, int, s, 32, 4, poly, p, 8, 16, expected_q_s32_8);
  TEST_VREINTERPRET(q, int, s, 32, 4, poly, p, 16, 8, expected_q_s32_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, int, s, 32, 4, float, f, 16, 8, expected_q_s32_10);
#endif

  /* vreinterpretq_s64_xx.  */
  TEST_VREINTERPRET(q, int, s, 64, 2, int, s, 8, 16, expected_q_s64_1);
  TEST_VREINTERPRET(q, int, s, 64, 2, int, s, 16, 8, expected_q_s64_2);
  TEST_VREINTERPRET(q, int, s, 64, 2, int, s, 32, 4, expected_q_s64_3);
  TEST_VREINTERPRET(q, int, s, 64, 2, uint, u, 8, 16, expected_q_s64_4);
  TEST_VREINTERPRET(q, int, s, 64, 2, uint, u, 16, 8, expected_q_s64_5);
  TEST_VREINTERPRET(q, int, s, 64, 2, uint, u, 32, 4, expected_q_s64_6);
  TEST_VREINTERPRET(q, int, s, 64, 2, uint, u, 64, 2, expected_q_s64_7);
  TEST_VREINTERPRET(q, int, s, 64, 2, poly, p, 8, 16, expected_q_s64_8);
  TEST_VREINTERPRET(q, int, s, 64, 2, poly, p, 16, 8, expected_q_s64_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, int, s, 64, 2, float, f, 16, 8, expected_q_s64_10);
#endif

  /* vreinterpretq_u8_xx.  */
  TEST_VREINTERPRET(q, uint, u, 8, 16, int, s, 8, 16, expected_q_u8_1);
  TEST_VREINTERPRET(q, uint, u, 8, 16, int, s, 16, 8, expected_q_u8_2);
  TEST_VREINTERPRET(q, uint, u, 8, 16, int, s, 32, 4, expected_q_u8_3);
  TEST_VREINTERPRET(q, uint, u, 8, 16, int, s, 64, 2, expected_q_u8_4);
  TEST_VREINTERPRET(q, uint, u, 8, 16, uint, u, 16, 8, expected_q_u8_5);
  TEST_VREINTERPRET(q, uint, u, 8, 16, uint, u, 32, 4, expected_q_u8_6);
  TEST_VREINTERPRET(q, uint, u, 8, 16, uint, u, 64, 2, expected_q_u8_7);
  TEST_VREINTERPRET(q, uint, u, 8, 16, poly, p, 8, 16, expected_q_u8_8);
  TEST_VREINTERPRET(q, uint, u, 8, 16, poly, p, 16, 8, expected_q_u8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, uint, u, 8, 16, float, f, 16, 8, expected_q_u8_10);
#endif

  /* vreinterpretq_u16_xx.  */
  TEST_VREINTERPRET(q, uint, u, 16, 8, int, s, 8, 16, expected_q_u16_1);
  TEST_VREINTERPRET(q, uint, u, 16, 8, int, s, 16, 8, expected_q_u16_2);
  TEST_VREINTERPRET(q, uint, u, 16, 8, int, s, 32, 4, expected_q_u16_3);
  TEST_VREINTERPRET(q, uint, u, 16, 8, int, s, 64, 2, expected_q_u16_4);
  TEST_VREINTERPRET(q, uint, u, 16, 8, uint, u, 8, 16, expected_q_u16_5);
  TEST_VREINTERPRET(q, uint, u, 16, 8, uint, u, 32, 4, expected_q_u16_6);
  TEST_VREINTERPRET(q, uint, u, 16, 8, uint, u, 64, 2, expected_q_u16_7);
  TEST_VREINTERPRET(q, uint, u, 16, 8, poly, p, 8, 16, expected_q_u16_8);
  TEST_VREINTERPRET(q, uint, u, 16, 8, poly, p, 16, 8, expected_q_u16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, uint, u, 16, 8, float, f, 16, 8, expected_q_u16_10);
#endif

  /* vreinterpretq_u32_xx.  */
  TEST_VREINTERPRET(q, uint, u, 32, 4, int, s, 8, 16, expected_q_u32_1);
  TEST_VREINTERPRET(q, uint, u, 32, 4, int, s, 16, 8, expected_q_u32_2);
  TEST_VREINTERPRET(q, uint, u, 32, 4, int, s, 32, 4, expected_q_u32_3);
  TEST_VREINTERPRET(q, uint, u, 32, 4, int, s, 64, 2, expected_q_u32_4);
  TEST_VREINTERPRET(q, uint, u, 32, 4, uint, u, 8, 16, expected_q_u32_5);
  TEST_VREINTERPRET(q, uint, u, 32, 4, uint, u, 16, 8, expected_q_u32_6);
  TEST_VREINTERPRET(q, uint, u, 32, 4, uint, u, 64, 2, expected_q_u32_7);
  TEST_VREINTERPRET(q, uint, u, 32, 4, poly, p, 8, 16, expected_q_u32_8);
  TEST_VREINTERPRET(q, uint, u, 32, 4, poly, p, 16, 8, expected_q_u32_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, uint, u, 32, 4, float, f, 16, 8, expected_q_u32_10);
#endif

  /* vreinterpretq_u64_xx.  */
  TEST_VREINTERPRET(q, uint, u, 64, 2, int, s, 8, 16, expected_q_u64_1);
  TEST_VREINTERPRET(q, uint, u, 64, 2, int, s, 16, 8, expected_q_u64_2);
  TEST_VREINTERPRET(q, uint, u, 64, 2, int, s, 32, 4, expected_q_u64_3);
  TEST_VREINTERPRET(q, uint, u, 64, 2, int, s, 64, 2, expected_q_u64_4);
  TEST_VREINTERPRET(q, uint, u, 64, 2, uint, u, 8, 16, expected_q_u64_5);
  TEST_VREINTERPRET(q, uint, u, 64, 2, uint, u, 16, 8, expected_q_u64_6);
  TEST_VREINTERPRET(q, uint, u, 64, 2, uint, u, 32, 4, expected_q_u64_7);
  TEST_VREINTERPRET(q, uint, u, 64, 2, poly, p, 8, 16, expected_q_u64_8);
  TEST_VREINTERPRET(q, uint, u, 64, 2, poly, p, 16, 8, expected_q_u64_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET(q, uint, u, 64, 2, float, f, 16, 8, expected_q_u64_10);
#endif

  /* vreinterpretq_p8_xx.  */
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, int, s, 8, 16, expected_q_p8_1);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, int, s, 16, 8, expected_q_p8_2);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, int, s, 32, 4, expected_q_p8_3);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, int, s, 64, 2, expected_q_p8_4);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, uint, u, 8, 16, expected_q_p8_5);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, uint, u, 16, 8, expected_q_p8_6);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, uint, u, 32, 4, expected_q_p8_7);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, uint, u, 64, 2, expected_q_p8_8);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, poly, p, 16, 8, expected_q_p8_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, float, f, 16, 8, expected_q_p8_10);
#endif

  /* vreinterpretq_p16_xx.  */
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, int, s, 8, 16, expected_q_p16_1);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, int, s, 16, 8, expected_q_p16_2);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, int, s, 32, 4, expected_q_p16_3);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, int, s, 64, 2, expected_q_p16_4);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, uint, u, 8, 16, expected_q_p16_5);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, uint, u, 16, 8, expected_q_p16_6);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, uint, u, 32, 4, expected_q_p16_7);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, uint, u, 64, 2, expected_q_p16_8);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, poly, p, 8, 16, expected_q_p16_9);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, float, f, 16, 8, expected_q_p16_10);
#endif

  /* vreinterpret_f32_xx.  */
  TEST_VREINTERPRET_FP(, float, f, 32, 2, int, s, 8, 8, expected_f32_1);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, int, s, 16, 4, expected_f32_2);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, int, s, 32, 2, expected_f32_3);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, int, s, 64, 1, expected_f32_4);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, uint, u, 8, 8, expected_f32_5);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, uint, u, 16, 4, expected_f32_6);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, uint, u, 32, 2, expected_f32_7);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, uint, u, 64, 1, expected_f32_8);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, poly, p, 8, 8, expected_f32_9);
  TEST_VREINTERPRET_FP(, float, f, 32, 2, poly, p, 16, 4, expected_f32_10);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_FP(, float, f, 32, 2, float, f, 16, 4, expected_f32_11);
#endif

  /* vreinterpretq_f32_xx.  */
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, int, s, 8, 16, expected_q_f32_1);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, int, s, 16, 8, expected_q_f32_2);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, int, s, 32, 4, expected_q_f32_3);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, int, s, 64, 2, expected_q_f32_4);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, uint, u, 8, 16, expected_q_f32_5);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, uint, u, 16, 8, expected_q_f32_6);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, uint, u, 32, 4, expected_q_f32_7);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, uint, u, 64, 2, expected_q_f32_8);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, poly, p, 8, 16, expected_q_f32_9);
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, poly, p, 16, 8, expected_q_f32_10);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_FP(q, float, f, 32, 4, float, f, 16, 8, expected_q_f32_11);
#endif

  /* vreinterpret_xx_f32.  */
  TEST_VREINTERPRET(, int, s, 8, 8, float, f, 32, 2, expected_xx_f32_1);
  TEST_VREINTERPRET(, int, s, 16, 4, float, f, 32, 2, expected_xx_f32_2);
  TEST_VREINTERPRET(, int, s, 32, 2, float, f, 32, 2, expected_xx_f32_3);
  TEST_VREINTERPRET(, int, s, 64, 1, float, f, 32, 2, expected_xx_f32_4);
  TEST_VREINTERPRET(, uint, u, 8, 8, float, f, 32, 2, expected_xx_f32_5);
  TEST_VREINTERPRET(, uint, u, 16, 4, float, f, 32, 2, expected_xx_f32_6);
  TEST_VREINTERPRET(, uint, u, 32, 2, float, f, 32, 2, expected_xx_f32_7);
  TEST_VREINTERPRET(, uint, u, 64, 1, float, f, 32, 2, expected_xx_f32_8);
  TEST_VREINTERPRET_POLY(, poly, p, 8, 8, float, f, 32, 2, expected_xx_f32_9);
  TEST_VREINTERPRET_POLY(, poly, p, 16, 4, float, f, 32, 2, expected_xx_f32_10);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_FP(, float, f, 16, 4, float, f, 32, 2, expected_xx_f32_11);
#endif

  /* vreinterpretq_xx_f32.  */
  TEST_VREINTERPRET(q, int, s, 8, 16, float, f, 32, 4, expected_q_xx_f32_1);
  TEST_VREINTERPRET(q, int, s, 16, 8, float, f, 32, 4, expected_q_xx_f32_2);
  TEST_VREINTERPRET(q, int, s, 32, 4, float, f, 32, 4, expected_q_xx_f32_3);
  TEST_VREINTERPRET(q, int, s, 64, 2, float, f, 32, 4, expected_q_xx_f32_4);
  TEST_VREINTERPRET(q, uint, u, 8, 16, float, f, 32, 4, expected_q_xx_f32_5);
  TEST_VREINTERPRET(q, uint, u, 16, 8, float, f, 32, 4, expected_q_xx_f32_6);
  TEST_VREINTERPRET(q, uint, u, 32, 4, float, f, 32, 4, expected_q_xx_f32_7);
  TEST_VREINTERPRET(q, uint, u, 64, 2, float, f, 32, 4, expected_q_xx_f32_8);
  TEST_VREINTERPRET_POLY(q, poly, p, 8, 16, float, f, 32, 4, expected_q_xx_f32_9);
  TEST_VREINTERPRET_POLY(q, poly, p, 16, 8, float, f, 32, 4, expected_q_xx_f32_10);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, float, f, 32, 4, expected_q_xx_f32_11);

  /* vreinterpret_f16_xx.  */
  TEST_VREINTERPRET_FP(, float, f, 16, 4, int, s, 8, 8, expected_f16_1);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, int, s, 16, 4, expected_f16_2);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, int, s, 32, 2, expected_f16_3);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, int, s, 64, 1, expected_f16_4);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, uint, u, 8, 8, expected_f16_5);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, uint, u, 16, 4, expected_f16_6);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, uint, u, 32, 2, expected_f16_7);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, uint, u, 64, 1, expected_f16_8);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, poly, p, 8, 8, expected_f16_9);
  TEST_VREINTERPRET_FP(, float, f, 16, 4, poly, p, 16, 4, expected_f16_10);

  /* vreinterpretq_f16_xx.  */
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, int, s, 8, 16, expected_q_f16_1);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, int, s, 16, 8, expected_q_f16_2);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, int, s, 32, 4, expected_q_f16_3);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, int, s, 64, 2, expected_q_f16_4);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, uint, u, 8, 16, expected_q_f16_5);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, uint, u, 16, 8, expected_q_f16_6);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, uint, u, 32, 4, expected_q_f16_7);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, uint, u, 64, 2, expected_q_f16_8);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, poly, p, 8, 16, expected_q_f16_9);
  TEST_VREINTERPRET_FP(q, float, f, 16, 8, poly, p, 16, 8, expected_q_f16_10);
#endif
}

int main (void)
{
  exec_vreinterpret ();
  return 0;
}
