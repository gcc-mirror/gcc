#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf7, 0x11, 0x11, 0x11,
				       0x11, 0x11, 0x11, 0x11 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff3, 0x22, 0x22, 0x22 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0x33 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf6, 0xf7, 0x55, 0x55,
					0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff2, 0xfff3, 0x66, 0x66 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff1, 0x77 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf6, 0xf7, 0x55, 0x55,
					0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff2, 0xfff3, 0x66, 0x66 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xf5, 0xf6, 0xf7, 0x77,
					   0x77, 0x77, 0x77, 0x77 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 4) [] = { 0xcb00, 0xca80,
					       0x4b4d, 0x4b4d };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1700000, 0x42066666 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xfe, 0xff, 0x11, 0x11,
					0x11, 0x11, 0x11, 0x11,
					0x11, 0x11, 0x11, 0x11,
					0x11, 0x11, 0x11, 0x11 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff7, 0x22, 0x22, 0x22,
					0x22, 0x22, 0x22, 0x22 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff3, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff1, 0x44 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xfc, 0xfd, 0xfe, 0xff,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff6, 0xfff7, 0x66, 0x66,
					 0x66, 0x66, 0x66, 0x66 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff3, 0x77, 0x77, 0x77 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff1, 0x88 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xfc, 0xfd, 0xfe, 0xff,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff6, 0xfff7, 0x66, 0x66,
					 0x66, 0x66, 0x66, 0x66 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xf9, 0xfa, 0xfb, 0xfc,
					    0xfd, 0xfe, 0xff, 0xaa,
					    0xaa, 0xaa, 0xaa, 0xaa,
					    0xaa, 0xaa, 0xaa, 0xaa };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xc880, 0x4b4d,
					       0x4b4d, 0x4b4d,
					       0x4b4d, 0x4b4d,
					       0x4b4d, 0x4b4d };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1500000, 0x4204cccd,
					   0x4204cccd, 0x4204cccd };

#define TEST_MSG "VEXT/VEXTQ"
void exec_vext (void)
{
  /* vector_res = vext(vector1,vector2,offset), then store the result.  */
#define TEST_VEXT(Q, T1, T2, W, N, V)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vext##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N),			\
		      V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);
#if MFLOAT8_SUPPORTED
  VLOAD(vector1, buffer, , mfloat, mf, 8, 8);
  VLOAD(vector1, buffer, q, mfloat, mf, 8, 16);
#endif
#ifdef FP16_SUPPORTED
  VLOAD(vector1, buffer, , float, f, 16, 4);
  VLOAD(vector1, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, float, f, 32, 4);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , int, s, 64, 1, 0x44);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , uint, u, 64, 1, 0x88);
  VDUP(vector2, , poly, p, 8, 8, 0x55);
  VDUP(vector2, , poly, p, 16, 4, 0x66);
  MFLOAT8_ONLY(VDUP(vector2, , mfloat, mf, 8, 8, MFLOAT8(0x77)));
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
  MFLOAT8_ONLY(VDUP(vector2, q, mfloat, mf, 8, 16, MFLOAT8(0xaa)));
#if defined (FP16_SUPPORTED)
  VDUP (vector2, q, float, f, 16, 8, 14.6f);
#endif
  VDUP(vector2, q, float, f, 32, 4, 33.2f);

  /* Choose arbitrary extract offsets.  */
  TEST_VEXT(, int, s, 8, 8, 7);
  TEST_VEXT(, int, s, 16, 4, 3);
  TEST_VEXT(, int, s, 32, 2, 1);
  TEST_VEXT(, int, s, 64, 1, 0);
  TEST_VEXT(, uint, u, 8, 8, 6);
  TEST_VEXT(, uint, u, 16, 4, 2);
  TEST_VEXT(, uint, u, 32, 2, 1);
  TEST_VEXT(, uint, u, 64, 1, 0);
  TEST_VEXT(, poly, p, 8, 8, 6);
  TEST_VEXT(, poly, p, 16, 4, 2);
  MFLOAT8_ONLY(TEST_VEXT(, mfloat, mf, 8, 8, 5));
#if defined (FP16_SUPPORTED)
  TEST_VEXT(, float, f, 16, 4, 2);
#endif
  TEST_VEXT(, float, f, 32, 2, 1);

  TEST_VEXT(q, int, s, 8, 16, 14);
  TEST_VEXT(q, int, s, 16, 8, 7);
  TEST_VEXT(q, int, s, 32, 4, 3);
  TEST_VEXT(q, int, s, 64, 2, 1);
  TEST_VEXT(q, uint, u, 8, 16, 12);
  TEST_VEXT(q, uint, u, 16, 8, 6);
  TEST_VEXT(q, uint, u, 32, 4, 3);
  TEST_VEXT(q, uint, u, 64, 2, 1);
  TEST_VEXT(q, poly, p, 8, 16, 12);
  TEST_VEXT(q, poly, p, 16, 8, 6);
  MFLOAT8_ONLY(TEST_VEXT(q, mfloat, mf, 8, 16, 9));
#if defined (FP16_SUPPORTED)
  TEST_VEXT(q, float, f, 16, 8, 7);
#endif
  TEST_VEXT(q, float, f, 32, 4, 3);

#if defined (FP16_SUPPORTED)
  CHECK_RESULTS (TEST_MSG, "");
#else
  CHECK_RESULTS_NO_FP16 (TEST_MSG, "");
#endif
}

int main (void)
{
  exec_vext ();
  return 0;
}
