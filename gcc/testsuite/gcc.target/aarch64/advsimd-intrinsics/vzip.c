#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results splitted in several chunks.  */
/* Chunk 0.  */
VECT_VAR_DECL(expected0,int,8,8) [] = { 0xf0, 0xf4, 0x11, 0x11,
					0xf1, 0xf5, 0x11, 0x11 };
VECT_VAR_DECL(expected0,int,16,4) [] = { 0xfff0, 0xfff2,
					 0x22, 0x22 };
VECT_VAR_DECL(expected0,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected0,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected0,uint,8,8) [] = { 0xf0, 0xf4, 0x55, 0x55,
					 0xf1, 0xf5, 0x55, 0x55 };
VECT_VAR_DECL(expected0,uint,16,4) [] = { 0xfff0, 0xfff2,
					  0x66, 0x66 };
VECT_VAR_DECL(expected0,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected0,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected0,poly,8,8) [] = { 0xf0, 0xf4, 0x55, 0x55,
					 0xf1, 0xf5, 0x55, 0x55 };
VECT_VAR_DECL(expected0,poly,16,4) [] = { 0xfff0, 0xfff2,
					  0x66, 0x66 };
VECT_VAR_DECL(expected0,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected0,int,8,16) [] = { 0xf0, 0xf8, 0x11, 0x11,
					 0xf1, 0xf9, 0x11, 0x11,
					 0xf2, 0xfa, 0x11, 0x11,
					 0xf3, 0xfb, 0x11, 0x11 };
VECT_VAR_DECL(expected0,int,16,8) [] = { 0xfff0, 0xfff4, 0x22, 0x22,
					 0xfff1, 0xfff5, 0x22, 0x22 };
VECT_VAR_DECL(expected0,int,32,4) [] = { 0xfffffff0, 0xfffffff2,
					 0x33, 0x33 };
VECT_VAR_DECL(expected0,int,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected0,uint,8,16) [] = { 0xf0, 0xf8, 0x55, 0x55,
					  0xf1, 0xf9, 0x55, 0x55,
					  0xf2, 0xfa, 0x55, 0x55,
					  0xf3, 0xfb, 0x55, 0x55 };
VECT_VAR_DECL(expected0,uint,16,8) [] = { 0xfff0, 0xfff4, 0x66, 0x66,
					  0xfff1, 0xfff5, 0x66, 0x66 };
VECT_VAR_DECL(expected0,uint,32,4) [] = { 0xfffffff0, 0xfffffff2,
					  0x77, 0x77 };
VECT_VAR_DECL(expected0,uint,64,2) [] = { 0x3333333333333333,
					  0x3333333333333333 };
VECT_VAR_DECL(expected0,poly,8,16) [] = { 0xf0, 0xf8, 0x55, 0x55,
					  0xf1, 0xf9, 0x55, 0x55,
					  0xf2, 0xfa, 0x55, 0x55,
					  0xf3, 0xfb, 0x55, 0x55 };
VECT_VAR_DECL(expected0,poly,16,8) [] = { 0xfff0, 0xfff4, 0x66, 0x66,
					  0xfff1, 0xfff5, 0x66, 0x66 };
VECT_VAR_DECL(expected0,hfloat,32,4) [] = { 0xc1800000, 0xc1600000,
					    0x42073333, 0x42073333 };

/* Chunk 1.  */
VECT_VAR_DECL(expected1,int,8,8) [] = { 0xf2, 0xf6, 0x11, 0x11,
					0xf3, 0xf7, 0x11, 0x11 };
VECT_VAR_DECL(expected1,int,16,4) [] = { 0xfff1, 0xfff3,
					 0x22, 0x22 };
VECT_VAR_DECL(expected1,int,32,2) [] = { 0x33, 0x33 };
VECT_VAR_DECL(expected1,int,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected1,uint,8,8) [] = { 0xf2, 0xf6, 0x55, 0x55,
					 0xf3, 0xf7, 0x55, 0x55 };
VECT_VAR_DECL(expected1,uint,16,4) [] = { 0xfff1, 0xfff3,
					  0x66, 0x66 };
VECT_VAR_DECL(expected1,uint,32,2) [] = { 0x77, 0x77 };
VECT_VAR_DECL(expected1,uint,64,1) [] = { 0x3333333333333333 };
VECT_VAR_DECL(expected1,poly,8,8) [] = { 0xf2, 0xf6, 0x55, 0x55,
					 0xf3, 0xf7, 0x55, 0x55 };
VECT_VAR_DECL(expected1,poly,16,4) [] = { 0xfff1, 0xfff3,
					  0x66, 0x66 };
VECT_VAR_DECL(expected1,hfloat,32,2) [] = { 0x42066666, 0x42066666 };
VECT_VAR_DECL(expected1,int,8,16) [] = { 0xf4, 0xfc, 0x11, 0x11,
					 0xf5, 0xfd, 0x11, 0x11,
					 0xf6, 0xfe, 0x11, 0x11,
					 0xf7, 0xff, 0x11, 0x11 };
VECT_VAR_DECL(expected1,int,16,8) [] = { 0xfff2, 0xfff6, 0x22, 0x22,
					 0xfff3, 0xfff7, 0x22, 0x22 };
VECT_VAR_DECL(expected1,int,32,4) [] = { 0xfffffff1, 0xfffffff3,
					 0x33, 0x33 };
VECT_VAR_DECL(expected1,int,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected1,uint,8,16) [] = { 0xf4, 0xfc, 0x55, 0x55,
					  0xf5, 0xfd, 0x55, 0x55,
					  0xf6, 0xfe, 0x55, 0x55,
					  0xf7, 0xff, 0x55, 0x55 };
VECT_VAR_DECL(expected1,uint,16,8) [] = { 0xfff2, 0xfff6, 0x66, 0x66,
					  0xfff3, 0xfff7, 0x66, 0x66 };
VECT_VAR_DECL(expected1,uint,32,4) [] = { 0xfffffff1, 0xfffffff3,
					  0x77, 0x77 };
VECT_VAR_DECL(expected1,uint,64,2) [] = { 0x3333333333333333,
					  0x3333333333333333 };
VECT_VAR_DECL(expected1,poly,8,16) [] = { 0xf4, 0xfc, 0x55, 0x55,
					  0xf5, 0xfd, 0x55, 0x55,
					  0xf6, 0xfe, 0x55, 0x55,
					  0xf7, 0xff, 0x55, 0x55 };
VECT_VAR_DECL(expected1,poly,16,8) [] = { 0xfff2, 0xfff6, 0x66, 0x66,
					  0xfff3, 0xfff7, 0x66, 0x66 };
VECT_VAR_DECL(expected1,hfloat,32,4) [] = { 0xc1700000, 0xc1500000,
					    0x42073333, 0x42073333 };

#ifndef INSN_NAME
#define INSN_NAME vzip
#define TEST_MSG "VZIP/VZIPQ"
#endif

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* In this case, output variables are arrays of vectors.  */
#define DECL_VZIP(T1, W, N)						\
  VECT_ARRAY_TYPE(T1, W, N, 2) VECT_ARRAY_VAR(result_vec, T1, W, N, 2);	\
  VECT_VAR_DECL(result_bis, T1, W, N)[2 * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behaviour. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
#define TEST_VZIP(INSN, Q, T1, T2, W, N)				\
  VECT_ARRAY_VAR(result_vec, T1, W, N, 2) =				\
    INSN##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst2##Q##_##T2##W(VECT_VAR(result_bis, T1, W, N),			\
		    VECT_ARRAY_VAR(result_vec, T1, W, N, 2));		\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis, T1, W, N),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[X].  */
#define TEST_EXTRA_CHUNK(T1, W, N, X)					\
  memcpy(VECT_VAR(result, T1, W, N), &(VECT_VAR(result_bis, T1, W, N)[X*N]), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);

  /* We don't need 64 bits variants.  */
#define DECL_ALL_VZIP()				\
  DECL_VZIP(int, 8, 8);				\
  DECL_VZIP(int, 16, 4);			\
  DECL_VZIP(int, 32, 2);			\
  DECL_VZIP(uint, 8, 8);			\
  DECL_VZIP(uint, 16, 4);			\
  DECL_VZIP(uint, 32, 2);			\
  DECL_VZIP(poly, 8, 8);			\
  DECL_VZIP(poly, 16, 4);			\
  DECL_VZIP(float, 32, 2);			\
  DECL_VZIP(int, 8, 16);			\
  DECL_VZIP(int, 16, 8);			\
  DECL_VZIP(int, 32, 4);			\
  DECL_VZIP(uint, 8, 16);			\
  DECL_VZIP(uint, 16, 8);			\
  DECL_VZIP(uint, 32, 4);			\
  DECL_VZIP(poly, 8, 16);			\
  DECL_VZIP(poly, 16, 8);			\
  DECL_VZIP(float, 32, 4)

  DECL_ALL_VZIP();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, float, f, 32, 4);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , poly, p, 8, 8, 0x55);
  VDUP(vector2, , poly, p, 16, 4, 0x66);
  VDUP(vector2, , float, f, 32, 2, 33.6f);

  VDUP(vector2, q, int, s, 8, 16, 0x11);
  VDUP(vector2, q, int, s, 16, 8, 0x22);
  VDUP(vector2, q, int, s, 32, 4, 0x33);
  VDUP(vector2, q, uint, u, 8, 16, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x66);
  VDUP(vector2, q, uint, u, 32, 4, 0x77);
  VDUP(vector2, q, poly, p, 8, 16, 0x55);
  VDUP(vector2, q, poly, p, 16, 8, 0x66);
  VDUP(vector2, q, float, f, 32, 4, 33.8f);

#define TEST_ALL_VZIP(INSN)			\
  TEST_VZIP(INSN, , int, s, 8, 8);		\
  TEST_VZIP(INSN, , int, s, 16, 4);		\
  TEST_VZIP(INSN, , int, s, 32, 2);		\
  TEST_VZIP(INSN, , uint, u, 8, 8);		\
  TEST_VZIP(INSN, , uint, u, 16, 4);		\
  TEST_VZIP(INSN, , uint, u, 32, 2);		\
  TEST_VZIP(INSN, , poly, p, 8, 8);		\
  TEST_VZIP(INSN, , poly, p, 16, 4);		\
  TEST_VZIP(INSN, , float, f, 32, 2);		\
  TEST_VZIP(INSN, q, int, s, 8, 16);		\
  TEST_VZIP(INSN, q, int, s, 16, 8);		\
  TEST_VZIP(INSN, q, int, s, 32, 4);		\
  TEST_VZIP(INSN, q, uint, u, 8, 16);		\
  TEST_VZIP(INSN, q, uint, u, 16, 8);		\
  TEST_VZIP(INSN, q, uint, u, 32, 4);		\
  TEST_VZIP(INSN, q, poly, p, 8, 16);		\
  TEST_VZIP(INSN, q, poly, p, 16, 8);		\
  TEST_VZIP(INSN, q, float, f, 32, 4)

#define TEST_ALL_EXTRA_CHUNKS()			\
  TEST_EXTRA_CHUNK(int, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(int, 16, 4, 1);		\
  TEST_EXTRA_CHUNK(int, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(uint, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(uint, 16, 4, 1);		\
  TEST_EXTRA_CHUNK(uint, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(poly, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(poly, 16, 4, 1);		\
  TEST_EXTRA_CHUNK(float, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(int, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(int, 16, 8, 1);		\
  TEST_EXTRA_CHUNK(int, 32, 4, 1);		\
  TEST_EXTRA_CHUNK(uint, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(uint, 16, 8, 1);		\
  TEST_EXTRA_CHUNK(uint, 32, 4, 1);		\
  TEST_EXTRA_CHUNK(poly, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(poly, 16, 8, 1);		\
  TEST_EXTRA_CHUNK(float, 32, 4, 1)

  clean_results ();

  /* Execute the tests.  */
  TEST_ALL_VZIP(INSN_NAME);

  CHECK_RESULTS_NAMED (TEST_MSG, expected0, "(chunk 0)");

  TEST_ALL_EXTRA_CHUNKS();
  CHECK_RESULTS_NAMED (TEST_MSG, expected1, "(chunk 1)");
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
