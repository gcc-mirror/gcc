#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* We test vdup and vmov in the same place since they are aliases.  */

/* Expected results.  */
/* Chunk 0.  */
VECT_VAR_DECL(expected0,int,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,int,16,4) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0 };
VECT_VAR_DECL(expected0,int,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected0,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected0,uint,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					 0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,uint,16,4) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0 };
VECT_VAR_DECL(expected0,uint,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected0,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected0,poly,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					 0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,poly,16,4) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected0,hmfloat,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					    0xf0, 0xf0, 0xf0, 0xf0 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected0, hfloat, 16, 4) [] = { 0xcc00, 0xcc00,
						0xcc00, 0xcc00 };
#endif
VECT_VAR_DECL(expected0,hfloat,32,2) [] = { 0xc1800000, 0xc1800000 };
VECT_VAR_DECL(expected0,int,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					 0xf0, 0xf0, 0xf0, 0xf0,
					 0xf0, 0xf0, 0xf0, 0xf0,
					 0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,int,16,8) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0,
					 0xfff0, 0xfff0, 0xfff0, 0xfff0 };
VECT_VAR_DECL(expected0,int,32,4) [] = { 0xfffffff0, 0xfffffff0,
					 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected0,int,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff0 };
VECT_VAR_DECL(expected0,uint,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,uint,16,8) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0,
					  0xfff0, 0xfff0, 0xfff0, 0xfff0 };
VECT_VAR_DECL(expected0,uint,32,4) [] = { 0xfffffff0, 0xfffffff0,
					  0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected0,uint,64,2) [] = { 0xfffffffffffffff0,
					  0xfffffffffffffff0 };
VECT_VAR_DECL(expected0,poly,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0,
					  0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected0,poly,16,8) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0,
					  0xfff0, 0xfff0, 0xfff0, 0xfff0 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected0,hmfloat,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					     0xf0, 0xf0, 0xf0, 0xf0,
					     0xf0, 0xf0, 0xf0, 0xf0,
					     0xf0, 0xf0, 0xf0, 0xf0 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected0, hfloat, 16, 8) [] = { 0xcc00, 0xcc00,
						0xcc00, 0xcc00,
						0xcc00, 0xcc00,
						0xcc00, 0xcc00 };
#endif
VECT_VAR_DECL(expected0,hfloat,32,4) [] = { 0xc1800000, 0xc1800000,
					    0xc1800000, 0xc1800000 };

/* Chunk 1.  */
VECT_VAR_DECL(expected1,int,8,8) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,int,16,4) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected1,int,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected1,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected1,uint,8,8) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					 0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,uint,16,4) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected1,uint,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected1,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected1,poly,8,8) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					 0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,poly,16,4) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected1,hmfloat,8,8) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					    0xf1, 0xf1, 0xf1, 0xf1 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected1, hfloat, 16, 4) [] = { 0xcb80, 0xcb80,
						0xcb80, 0xcb80 };
#endif
VECT_VAR_DECL(expected1,hfloat,32,2) [] = { 0xc1700000, 0xc1700000 };
VECT_VAR_DECL(expected1,int,8,16) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					 0xf1, 0xf1, 0xf1, 0xf1,
					 0xf1, 0xf1, 0xf1, 0xf1,
					 0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,int,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected1,int,32,4) [] = { 0xfffffff1, 0xfffffff1,
					 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected1,int,64,2) [] = { 0xfffffffffffffff1,
					 0xfffffffffffffff1 };
VECT_VAR_DECL(expected1,uint,8,16) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,uint,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					  0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected1,uint,32,4) [] = { 0xfffffff1, 0xfffffff1,
					  0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected1,uint,64,2) [] = { 0xfffffffffffffff1,
					  0xfffffffffffffff1 };
VECT_VAR_DECL(expected1,poly,8,16) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1,
					  0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected1,poly,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					  0xfff1, 0xfff1, 0xfff1, 0xfff1 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected1,hmfloat,8,16) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
					     0xf1, 0xf1, 0xf1, 0xf1,
					     0xf1, 0xf1, 0xf1, 0xf1,
					     0xf1, 0xf1, 0xf1, 0xf1 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected1, hfloat, 16, 8) [] = { 0xcb80, 0xcb80,
						0xcb80, 0xcb80,
						0xcb80, 0xcb80,
						0xcb80, 0xcb80 };
#endif
VECT_VAR_DECL(expected1,hfloat,32,4) [] = { 0xc1700000, 0xc1700000,
					    0xc1700000, 0xc1700000 };

/* Chunk 2.  */
VECT_VAR_DECL(expected2,int,8,8) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,int,16,4) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected2,int,32,2) [] = { 0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected2,int,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected2,uint,8,8) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					 0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,uint,16,4) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected2,uint,32,2) [] = { 0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected2,uint,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected2,poly,8,8) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					 0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,poly,16,4) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,8) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					    0xf2, 0xf2, 0xf2, 0xf2 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 4) [] = { 0xcb00, 0xcb00,
						0xcb00, 0xcb00 };
#endif
VECT_VAR_DECL(expected2,hfloat,32,2) [] = { 0xc1600000, 0xc1600000 };
VECT_VAR_DECL(expected2,int,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					 0xf2, 0xf2, 0xf2, 0xf2,
					 0xf2, 0xf2, 0xf2, 0xf2,
					 0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,int,16,8) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2,
					 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected2,int,32,4) [] = { 0xfffffff2, 0xfffffff2,
					 0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0xfffffffffffffff2,
					 0xfffffffffffffff2 };
VECT_VAR_DECL(expected2,uint,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,uint,16,8) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2,
					  0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0xfffffff2, 0xfffffff2,
					  0xfffffff2, 0xfffffff2 };
VECT_VAR_DECL(expected2,uint,64,2) [] = { 0xfffffffffffffff2,
					  0xfffffffffffffff2 };
VECT_VAR_DECL(expected2,poly,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2,
					  0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected2,poly,16,8) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2,
					  0xfff2, 0xfff2, 0xfff2, 0xfff2 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					     0xf2, 0xf2, 0xf2, 0xf2,
					     0xf2, 0xf2, 0xf2, 0xf2,
					     0xf2, 0xf2, 0xf2, 0xf2 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 8) [] = { 0xcb00, 0xcb00,
						0xcb00, 0xcb00,
						0xcb00, 0xcb00,
						0xcb00, 0xcb00 };
#endif
VECT_VAR_DECL(expected2,hfloat,32,4) [] = { 0xc1600000, 0xc1600000,
					    0xc1600000, 0xc1600000 };

#define TEST_MSG "VDUP/VDUPQ"
void exec_vdup_vmov (void)
{
  int i;

  /* Basic test: vec=vdup(x), then store the result.  */
#undef TEST_VDUP
#define TEST_VDUP(Q, T1, T2, W, N)					\
  VECT_VAR(vector, T1, W, N) =						\
    vdup##Q##_n_##T2##W(VECT_VAR(buffer_dup, T1, W, N)[i]);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector, T1, W, N))

  /* Basic test: vec=vmov(x), then store the result.  */
#define TEST_VMOV(Q, T1, T2, W, N)					\
  VECT_VAR(vector, T1, W, N) =						\
    vmov##Q##_n_##T2##W(VECT_VAR(buffer_dup, T1, W, N)[i]);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);

  /* Try to read different places from the input buffer.  */
  for (i=0; i< 3; i++) {
    clean_results ();

    TEST_VDUP(, int, s, 8, 8);
    TEST_VDUP(, int, s, 16, 4);
    TEST_VDUP(, int, s, 32, 2);
    TEST_VDUP(, int, s, 64, 1);
    TEST_VDUP(, uint, u, 8, 8);
    TEST_VDUP(, uint, u, 16, 4);
    TEST_VDUP(, uint, u, 32, 2);
    TEST_VDUP(, uint, u, 64, 1);
    TEST_VDUP(, poly, p, 8, 8);
    TEST_VDUP(, poly, p, 16, 4);
    MFLOAT8_ONLY(TEST_VDUP(, mfloat, mf, 8, 8));
#if defined (FP16_SUPPORTED)
    TEST_VDUP(, float, f, 16, 4);
#endif
    TEST_VDUP(, float, f, 32, 2);

    TEST_VDUP(q, int, s, 8, 16);
    TEST_VDUP(q, int, s, 16, 8);
    TEST_VDUP(q, int, s, 32, 4);
    TEST_VDUP(q, int, s, 64, 2);
    TEST_VDUP(q, uint, u, 8, 16);
    TEST_VDUP(q, uint, u, 16, 8);
    TEST_VDUP(q, uint, u, 32, 4);
    TEST_VDUP(q, uint, u, 64, 2);
    TEST_VDUP(q, poly, p, 8, 16);
    TEST_VDUP(q, poly, p, 16, 8);
    MFLOAT8_ONLY(TEST_VDUP(q, mfloat, mf, 8, 16));
#if defined (FP16_SUPPORTED)
    TEST_VDUP(q, float, f, 16, 8);
#endif
    TEST_VDUP(q, float, f, 32, 4);

#if defined (FP16_SUPPORTED)
    switch (i) {
    case 0:
      CHECK_RESULTS_NAMED (TEST_MSG, expected0, "");
      break;
    case 1:
      CHECK_RESULTS_NAMED (TEST_MSG, expected1, "");
      break;
    case 2:
      CHECK_RESULTS_NAMED (TEST_MSG, expected2, "");
      break;
    default:
      abort();
    }
#else
    switch (i) {
    case 0:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected0, "");
      break;
    case 1:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected1, "");
      break;
    case 2:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected2, "");
      break;
    default:
      abort();
    }
#endif
  }

  /* Do the same tests with vmov. Use the same expected results.  */
#undef TEST_MSG
#define TEST_MSG "VMOV/VMOVQ"
  for (i=0; i< 3; i++) {
    clean_results ();

    TEST_VMOV(, int, s, 8, 8);
    TEST_VMOV(, int, s, 16, 4);
    TEST_VMOV(, int, s, 32, 2);
    TEST_VMOV(, int, s, 64, 1);
    TEST_VMOV(, uint, u, 8, 8);
    TEST_VMOV(, uint, u, 16, 4);
    TEST_VMOV(, uint, u, 32, 2);
    TEST_VMOV(, uint, u, 64, 1);
    TEST_VMOV(, poly, p, 8, 8);
    TEST_VMOV(, poly, p, 16, 4);
    MFLOAT8_ONLY(TEST_VMOV(, mfloat, mf, 8, 8));
#if defined (FP16_SUPPORTED)
    TEST_VMOV(, float, f, 16, 4);
#endif
    TEST_VMOV(, float, f, 32, 2);

    TEST_VMOV(q, int, s, 8, 16);
    TEST_VMOV(q, int, s, 16, 8);
    TEST_VMOV(q, int, s, 32, 4);
    TEST_VMOV(q, int, s, 64, 2);
    TEST_VMOV(q, uint, u, 8, 16);
    TEST_VMOV(q, uint, u, 16, 8);
    TEST_VMOV(q, uint, u, 32, 4);
    TEST_VMOV(q, uint, u, 64, 2);
    TEST_VMOV(q, poly, p, 8, 16);
    TEST_VMOV(q, poly, p, 16, 8);
    MFLOAT8_ONLY(TEST_VMOV(q, mfloat, mf, 8, 16));
#if defined (FP16_SUPPORTED)
    TEST_VMOV(q, float, f, 16, 8);
#endif
    TEST_VMOV(q, float, f, 32, 4);

#if defined (FP16_SUPPORTED)
    switch (i) {
    case 0:
      CHECK_RESULTS_NAMED (TEST_MSG, expected0, "");
      break;
    case 1:
      CHECK_RESULTS_NAMED (TEST_MSG, expected1, "");
      break;
    case 2:
      CHECK_RESULTS_NAMED (TEST_MSG, expected2, "");
      break;
    default:
      abort();
    }
#else
    switch (i) {
    case 0:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected0, "");
      break;
    case 1:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected1, "");
      break;
    case 2:
      CHECK_RESULTS_NAMED_NO_FP16 (TEST_MSG, expected2, "");
      break;
    default:
      abort();
    }
#endif

  }
}

int main (void)
{
  exec_vdup_vmov ();
  return 0;
}
