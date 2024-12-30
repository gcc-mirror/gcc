/* { dg-do run } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0x11, 0xf1, 0x11,
				       0xf2, 0x11, 0xf3, 0x11 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0x22, 0xfff1, 0x22 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0x33 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0x55, 0xf1, 0x55,
					0xf2, 0x55, 0xf3, 0x55 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0x66, 0xfff1, 0x66 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0x77 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf0, 0x55, 0xf1, 0x55,
					0xf2, 0x55, 0xf3, 0x55 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff0, 0x66, 0xfff1, 0x66 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800000, 0x42066666 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xf0, 0xf9, 0xf1, 0xf9,
					   0xf2, 0xf9, 0xf3, 0xf9 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 4) [] = { 0xcc00, 0x4b4d,
					       0xcb80, 0x4b4d };
#endif
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0x11, 0xf1, 0x11,
					0xf2, 0x11, 0xf3, 0x11,
					0xf4, 0x11, 0xf5, 0x11,
					0xf6, 0x11, 0xf7, 0x11 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0x22, 0xfff1, 0x22,
					0xfff2, 0x22, 0xfff3, 0x22 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0x33,
					0xfffffff1, 0x33 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0,
					0x44 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0x55, 0xf1, 0x55,
					 0xf2, 0x55, 0xf3, 0x55,
					 0xf4, 0x55, 0xf5, 0x55,
					 0xf6, 0x55, 0xf7, 0x55 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0x66, 0xfff1, 0x66,
					 0xfff2, 0x66, 0xfff3, 0x66 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0x77,
					 0xfffffff1, 0x77 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0x88 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf0, 0x55, 0xf1, 0x55,
					 0xf2, 0x55, 0xf3, 0x55,
					 0xf4, 0x55, 0xf5, 0x55,
					 0xf6, 0x55, 0xf7, 0x55 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0x66, 0xfff1, 0x66,
					 0xfff2, 0x66, 0xfff3, 0x66 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xf0, 0xd6, 0xf1, 0xd6,
					    0xf2, 0xd6, 0xf3, 0xd6,
					    0xf4, 0xd6, 0xf5, 0xd6,
					    0xf6, 0xd6, 0xf7, 0xd6 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xcc00, 0x4b4d,
					       0xcb80, 0x4b4d,
					       0xcb00, 0x4b4d,
					       0xca80, 0x4b4d };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800000, 0x42073333,
					   0xc1700000, 0x42073333 };

#define TEST_MSG "VZIP1"
void exec_vzip_half (void)
{
#define TEST_VZIP(PART, Q, T1, T2, W, N)		\
  VECT_VAR(vector_res, T1, W, N) =			\
    vzip##PART##Q##_##T2##W(VECT_VAR(vector, T1, W, N),	\
		       VECT_VAR(vector2, T1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define TEST_VZIP1(Q, T1, T2, W, N) TEST_VZIP(1, Q, T1, T2, W, N)

  /* Input vector can only have 64 bits.  */
  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);

  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();
  /* We don't have vzip1_T64x1, so set expected to the clean value.  */
  CLEAN(expected, int, 64, 1);
  CLEAN(expected, uint, 64, 1);

  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if MFLOAT8_SUPPORTED
  VLOAD(vector, buffer, , mfloat, mf, 8, 8);
  VLOAD(vector, buffer, q, mfloat, mf, 8, 16);
#endif
#if defined (FP16_SUPPORTED)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);
  VLOAD(vector, buffer, q, float, f, 64, 2);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , poly, p, 8, 8, 0x55);
  VDUP(vector2, , poly, p, 16, 4, 0x66);
  MFLOAT8_ONLY(VDUP(vector2, , mfloat, mf, 8, 8, MFLOAT8(0xf9)));
#if defined (FP16_SUPPORTED)
  VDUP (vector2, , float, f, 16, 4, 14.6f);   /* 14.6f is 0x4b4d.  */
#endif
  VDUP(vector2, , float, f, 32, 2, 33.6f);

  VDUP(vector2, q, int, s, 8, 16, 0x11);
  VDUP(vector2, q, int, s, 16, 8, 0x22);
  VDUP(vector2, q, int, s, 32, 4, 0x33);
  VDUP(vector2, q, int, s, 64, 2, 0x44);
  VDUP(vector2, q, uint, u, 8, 16, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x66);
  VDUP(vector2, q, uint, u, 32, 4, 0x77);
  VDUP(vector2, q, uint, u, 64, 2, 0x88);
  VDUP(vector2, q, poly, p, 8, 16, 0x55);
  VDUP(vector2, q, poly, p, 16, 8, 0x66);
  MFLOAT8_ONLY(VDUP(vector2, q, mfloat, mf, 8, 16, MFLOAT8(0xd6)));
#if defined (FP16_SUPPORTED)
  VDUP (vector2, q, float, f, 16, 8, 14.6f);
#endif
  VDUP(vector2, q, float, f, 32, 4, 33.8f);
  VDUP(vector2, q, float, f, 64, 2, 33.8f);

  TEST_VZIP1(, int, s, 8, 8);
  TEST_VZIP1(, int, s, 16, 4);
  TEST_VZIP1(, int, s, 32, 2);
  TEST_VZIP1(, uint, u, 8, 8);
  TEST_VZIP1(, uint, u, 16, 4);
  TEST_VZIP1(, uint, u, 32, 2);
  TEST_VZIP1(, poly, p, 8, 8);
  TEST_VZIP1(, poly, p, 16, 4);
  MFLOAT8_ONLY(TEST_VZIP1(, mfloat, mf, 8, 8));
#if defined (FP16_SUPPORTED)
  TEST_VZIP1(, float, f, 16, 4);
#endif
  TEST_VZIP1(, float, f, 32, 2);

  TEST_VZIP1(q, int, s, 8, 16);
  TEST_VZIP1(q, int, s, 16, 8);
  TEST_VZIP1(q, int, s, 32, 4);
  TEST_VZIP1(q, int, s, 64, 2);
  TEST_VZIP1(q, uint, u, 8, 16);
  TEST_VZIP1(q, uint, u, 16, 8);
  TEST_VZIP1(q, uint, u, 32, 4);
  TEST_VZIP1(q, uint, u, 64, 2);
  TEST_VZIP1(q, poly, p, 8, 16);
  TEST_VZIP1(q, poly, p, 16, 8);
  MFLOAT8_ONLY(TEST_VZIP1(q, mfloat, mf, 8, 16));
#if defined (FP16_SUPPORTED)
  TEST_VZIP1(q, float, f, 16, 8);
#endif
  TEST_VZIP1(q, float, f, 32, 4);
  TEST_VZIP1(q, float, f, 64, 2);

#if defined (FP16_SUPPORTED)
  CHECK_RESULTS (TEST_MSG, "");
#else
  CHECK_RESULTS_NO_FP16 (TEST_MSG, "");
#endif

#undef TEST_MSG
#define TEST_MSG "VZIP2"

#define TEST_VZIP2(Q, T1, T2, W, N) TEST_VZIP(2, Q, T1, T2, W, N)

/* Expected results.  */
VECT_VAR_DECL(expected2,int,8,8) [] = { 0xf4, 0x11, 0xf5, 0x11,
					0xf6, 0x11, 0xf7, 0x11 };
VECT_VAR_DECL(expected2,int,16,4) [] = { 0xfff2, 0x22, 0xfff3, 0x22 };
VECT_VAR_DECL(expected2,int,32,2) [] = { 0xfffffff1, 0x33 };
VECT_VAR_DECL(expected2,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected2,uint,8,8) [] = { 0xf4, 0x55, 0xf5, 0x55,
					 0xf6, 0x55, 0xf7, 0x55 };
VECT_VAR_DECL(expected2,uint,16,4) [] = { 0xfff2, 0x66, 0xfff3, 0x66 };
VECT_VAR_DECL(expected2,uint,32,2) [] = { 0xfffffff1, 0x77 };
VECT_VAR_DECL(expected2,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected2,poly,8,8) [] = { 0xf4, 0x55, 0xf5, 0x55,
					 0xf6, 0x55, 0xf7, 0x55 };
VECT_VAR_DECL(expected2,poly,16,4) [] = { 0xfff2, 0x66, 0xfff3, 0x66 };
VECT_VAR_DECL(expected2,hfloat,32,2) [] = { 0xc1700000, 0x42066666 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,8) [] = { 0xf4, 0xf9, 0xf5, 0xf9,
					    0xf6, 0xf9, 0xf7, 0xf9 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 4) [] = { 0xcb00, 0x4b4d,
						0xca80, 0x4b4d };
#endif
VECT_VAR_DECL(expected2,int,8,16) [] = { 0xf8, 0x11, 0xf9, 0x11,
					 0xfa, 0x11, 0xfb, 0x11,
					 0xfc, 0x11, 0xfd, 0x11,
					 0xfe, 0x11, 0xff, 0x11 };
VECT_VAR_DECL(expected2,int,16,8) [] = { 0xfff4, 0x22, 0xfff5, 0x22,
					 0xfff6, 0x22, 0xfff7, 0x22 };
VECT_VAR_DECL(expected2,int,32,4) [] = { 0xfffffff2, 0x33,
					 0xfffffff3, 0x33 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0xfffffffffffffff1,
					 0x44 };
VECT_VAR_DECL(expected2,uint,8,16) [] = { 0xf8, 0x55, 0xf9, 0x55,
					  0xfa, 0x55, 0xfb, 0x55,
					  0xfc, 0x55, 0xfd, 0x55,
					  0xfe, 0x55, 0xff, 0x55 };
VECT_VAR_DECL(expected2,uint,16,8) [] = { 0xfff4, 0x66, 0xfff5, 0x66,
					  0xfff6, 0x66, 0xfff7, 0x66 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0xfffffff2, 0x77,
					  0xfffffff3, 0x77 };
VECT_VAR_DECL(expected2,uint,64,2) [] = { 0xfffffffffffffff1,
					  0x88 };
VECT_VAR_DECL(expected2,poly,8,16) [] = { 0xf8, 0x55, 0xf9, 0x55,
					  0xfa, 0x55, 0xfb, 0x55,
					  0xfc, 0x55, 0xfd, 0x55,
					  0xfe, 0x55, 0xff, 0x55 };
VECT_VAR_DECL(expected2,poly,16,8) [] = { 0xfff4, 0x66, 0xfff5, 0x66,
					  0xfff6, 0x66, 0xfff7, 0x66 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,16) [] = { 0xf8, 0xd6, 0xf9, 0xd6,
					     0xfa, 0xd6, 0xfb, 0xd6,
					     0xfc, 0xd6, 0xfd, 0xd6,
					     0xfe, 0xd6, 0xff, 0xd6 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 8) [] = { 0xca00, 0x4b4d,
						0xc980, 0x4b4d,
						0xc900, 0x4b4d,
						0xc880, 0x4b4d };
#endif
VECT_VAR_DECL(expected2,hfloat,32,4) [] = { 0xc1600000, 0x42073333,
					    0xc1500000, 0x42073333 };
  clean_results ();
  CLEAN(expected2, int, 64, 1);
  CLEAN(expected2, uint, 64, 1);

  TEST_VZIP2(, int, s, 8, 8);
  TEST_VZIP2(, int, s, 16, 4);
  TEST_VZIP2(, int, s, 32, 2);
  TEST_VZIP2(, uint, u, 8, 8);
  TEST_VZIP2(, uint, u, 16, 4);
  TEST_VZIP2(, uint, u, 32, 2);
  TEST_VZIP2(, poly, p, 8, 8);
  TEST_VZIP2(, poly, p, 16, 4);
  MFLOAT8_ONLY(TEST_VZIP2(, mfloat, mf, 8, 8));
#if defined (FP16_SUPPORTED)
  TEST_VZIP2(, float, f, 16, 4);
#endif
  TEST_VZIP2(, float, f, 32, 2);

  TEST_VZIP2(q, int, s, 8, 16);
  TEST_VZIP2(q, int, s, 16, 8);
  TEST_VZIP2(q, int, s, 32, 4);
  TEST_VZIP2(q, int, s, 64, 2);
  TEST_VZIP2(q, uint, u, 8, 16);
  TEST_VZIP2(q, uint, u, 16, 8);
  TEST_VZIP2(q, uint, u, 32, 4);
  TEST_VZIP2(q, uint, u, 64, 2);
  TEST_VZIP2(q, poly, p, 8, 16);
  TEST_VZIP2(q, poly, p, 16, 8);
  MFLOAT8_ONLY(TEST_VZIP2(q, mfloat, mf, 8, 16));
#if defined (FP16_SUPPORTED)
  TEST_VZIP2(q, float, f, 16, 8);
#endif
  TEST_VZIP2(q, float, f, 32, 4);
  TEST_VZIP2(q, float, f, 64, 2);

  CHECK_RESULTS_NAMED (TEST_MSG, expected2, "");
#if defined (FP16_SUPPORTED)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected2, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected2, "");
#endif
}

int main (void)
{
  exec_vzip_half ();
  return 0;
}
