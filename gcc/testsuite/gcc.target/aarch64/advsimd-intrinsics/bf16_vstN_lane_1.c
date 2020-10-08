/* { dg-do run { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vst2, chunk 0.  */
VECT_VAR_DECL(expected_st2_0,hbfloat,16,4) [] = { 0xABAB, 0x3210, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_0,hbfloat,16,8) [] = { 0xABAB, 0x3210, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst2, chunk 1.  */
VECT_VAR_DECL(expected_st2_1,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st2_1,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst3, chunk 0.  */
VECT_VAR_DECL(expected_st3_0,hbfloat,16,4) [] = { 0xABAB, 0x3210, 0xCAFE, 0x0 };
VECT_VAR_DECL(expected_st3_0,hbfloat,16,8) [] = { 0xABAB, 0x3210, 0xCAFE, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst3, chunk 1.  */
VECT_VAR_DECL(expected_st3_1,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_1,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst3, chunk 2.  */
VECT_VAR_DECL(expected_st3_2,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st3_2,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 0.  */
VECT_VAR_DECL(expected_st4_0,hbfloat,16,4) [] =
  { 0xABAB, 0x3210, 0xCAFE, 0x1234 };
VECT_VAR_DECL(expected_st4_0,hbfloat,16,8) [] =
  { 0xABAB, 0x3210, 0xCAFE, 0x1234, 0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 1.  */
VECT_VAR_DECL(expected_st4_1,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_1,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 2.  */
VECT_VAR_DECL(expected_st4_2,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_2,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					          0x0, 0x0, 0x0, 0x0 };

/* Expected results for vst4, chunk 3.  */
VECT_VAR_DECL(expected_st4_3,hbfloat,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_st4_3,hbfloat,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };

typedef union
{
  bfloat16_t bf16;
  uint16_t u16;
} bfloat16_u_t;

static bfloat16_t result_bfloat16x4[4];
static bfloat16_t result_bfloat16x8[8];

void exec_vstX_lane (void)
{
  bfloat16_u_t bfloat16_data[4];
  bfloat16_data[0].u16 = 0xABAB;
  bfloat16_data[1].u16 = 0x3210;
  bfloat16_data[2].u16 = 0xCAFE;
  bfloat16_data[3].u16 = 0x1234;

  bfloat16_t buffer_vld2_lane_bfloat16x2 [2] =
    { bfloat16_data[0].bf16,
      bfloat16_data[1].bf16 };
  bfloat16_t buffer_vld3_lane_bfloat16x3 [3] =
    { bfloat16_data[0].bf16,
      bfloat16_data[1].bf16,
      bfloat16_data[2].bf16 };
  bfloat16_t buffer_vld4_lane_bfloat16x4 [4] =
    { bfloat16_data[0].bf16,
      bfloat16_data[1].bf16,
      bfloat16_data[2].bf16,
      bfloat16_data[3].bf16 };

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

#define DUMMY_ARRAY(V, T, W, N, L) VECT_VAR_DECL(V,T,W,N)[N*L]

  DECL_VSTX_LANE(bfloat, 16, 4, 2);
  DECL_VSTX_LANE(bfloat, 16, 8, 2);
  DECL_VSTX_LANE(bfloat, 16, 4, 3);
  DECL_VSTX_LANE(bfloat, 16, 8, 3);
  DECL_VSTX_LANE(bfloat, 16, 4, 4);
  DECL_VSTX_LANE(bfloat, 16, 8, 4);

  DUMMY_ARRAY(buffer_src, bfloat, 16, 4, 4);
  DUMMY_ARRAY(buffer_src, bfloat, 16, 8, 4);

  /* Check vst2_lane/vst2q_lane.  */
  clean_results ();
  TEST_VSTX_LANE(, bfloat, bf, 16, 4, 2, 2);
  TEST_VSTX_LANE(q, bfloat, bf, 16, 8, 2, 6);

#undef CMT
#define CMT " (chunk 0)"
#undef TEST_MSG
#define TEST_MSG "VST2_LANE/VST2Q_LANE"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st2_0, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st2_0, CMT);
  TEST_EXTRA_CHUNK(bfloat, 16, 4, 2, 1);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 2, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st2_1, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st2_1, CMT);

  /* Check vst3_lane/vst3q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST3_LANE/VST3Q_LANE"
  TEST_VSTX_LANE(, bfloat, bf, 16, 4, 3, 2);
  TEST_VSTX_LANE(q, bfloat, bf, 16, 8, 3, 6);

#undef CMT
#define CMT " (chunk 0)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st3_0, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st3_0, CMT);

  TEST_EXTRA_CHUNK(bfloat, 16, 4, 3, 1);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 3, 1);


#undef CMT
#define CMT " (chunk 1)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st3_1, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st3_1, CMT);

  TEST_EXTRA_CHUNK(bfloat, 16, 4, 3, 2);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 3, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st3_2, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st3_2, CMT);

  /* Check vst4_lane/vst4q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST4_LANE/VST4Q_LANE"
  TEST_VSTX_LANE(, bfloat, bf, 16, 4, 4, 2);
  TEST_VSTX_LANE(q, bfloat, bf, 16, 8, 4, 6);

#undef CMT
#define CMT " (chunk 0)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st4_0, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st4_0, CMT);

  TEST_EXTRA_CHUNK(bfloat, 16, 4, 4, 1);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 4, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st4_1, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st4_1, CMT);

  TEST_EXTRA_CHUNK(bfloat, 16, 4, 4, 2);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 4, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st4_2, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st4_2, CMT);

  TEST_EXTRA_CHUNK(bfloat, 16, 4, 4, 3);
  TEST_EXTRA_CHUNK(bfloat, 16, 8, 4, 3);

#undef CMT
#define CMT " (chunk 3)"
  CHECK_FP(TEST_MSG, bfloat, 16, 4, PRIx16, expected_st4_3, CMT);
  CHECK_FP(TEST_MSG, bfloat, 16, 8, PRIx16, expected_st4_3, CMT);
}

int main (void)
{
  exec_vstX_lane ();
  return 0;
}
