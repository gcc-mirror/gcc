/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vqtbl1.  */
VECT_VAR_DECL(expected_vqtbl1,int,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					      0x0, 0x0, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbl1,uint,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					       0x0, 0x0, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbl1,poly,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					       0x0, 0x0, 0xf3, 0xf3 };

/* Expected results for vqtbl2.  */
VECT_VAR_DECL(expected_vqtbl2,int,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					      0xfa, 0x0, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbl2,uint,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					       0xfa, 0x0, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbl2,poly,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					       0xfa, 0x0, 0xf5, 0xf5 };

/* Expected results for vqtbl3.  */
VECT_VAR_DECL(expected_vqtbl3,int,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					      0xfe, 0xb, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbl3,uint,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					       0xfe, 0xb, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbl3,poly,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					       0xfe, 0xb, 0xf7, 0xf7 };

/* Expected results for vqtbl4.  */
VECT_VAR_DECL(expected_vqtbl4,int,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					      0x2, 0x13, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbl4,uint,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					       0x2, 0x13, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbl4,poly,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					       0x2, 0x13, 0xf9, 0xf9 };

/* Expected results for vqtbx1.  */
VECT_VAR_DECL(expected_vqtbx1,int,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					      0x33, 0x33, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbx1,uint,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					       0xcc, 0xcc, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbx1,poly,8,8) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
					       0xcc, 0xcc, 0xf3, 0xf3 };

/* Expected results for vqtbx2.  */
VECT_VAR_DECL(expected_vqtbx2,int,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					      0xfa, 0x33, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbx2,uint,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					       0xfa, 0xcc, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbx2,poly,8,8) [] = { 0x5, 0xf5, 0xf5, 0xf5,
					       0xfa, 0xcc, 0xf5, 0xf5 };

/* Expected results for vqtbx3.  */
VECT_VAR_DECL(expected_vqtbx3,int,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					      0xfe, 0xb, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbx3,uint,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					       0xfe, 0xb, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbx3,poly,8,8) [] = { 0xf, 0xf7, 0xf7, 0xf7,
					       0xfe, 0xb, 0xf7, 0xf7 };

/* Expected results for vqtbx4.  */
VECT_VAR_DECL(expected_vqtbx4,int,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					      0x2, 0x13, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbx4,uint,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					       0x2, 0x13, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbx4,poly,8,8) [] = { 0x19, 0xf9, 0xf9, 0xf9,
					       0x2, 0x13, 0xf9, 0xf9 };

/* Expected results for vqtbl1q.  */
VECT_VAR_DECL(expected_vqtbl1q,int,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						0x0, 0x0, 0xf3, 0xf3,
						0xf3, 0xf3, 0xf3, 0xf3,
						0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbl1q,uint,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						 0x0, 0x0, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbl1q,poly,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						 0x0, 0x0, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3 };

/* Expected results for vqtbl2q.  */
VECT_VAR_DECL(expected_vqtbl2q,int,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						0xfa, 0x0, 0xf5, 0xf5,
						0xf5, 0xf5, 0xf5, 0xf5,
						0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbl2q,uint,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						 0xfa, 0x0, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbl2q,poly,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						 0xfa, 0x0, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5 };

/* Expected results for vqtbl3q.  */
VECT_VAR_DECL(expected_vqtbl3q,int,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						0xfe, 0xb, 0xf7, 0xf7,
						0xf7, 0xf7, 0xf7, 0xf7,
						0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbl3q,uint,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						 0xfe, 0xb, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbl3q,poly,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						 0xfe, 0xb, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7 };

/* Expected results for vqtbl4q.  */
VECT_VAR_DECL(expected_vqtbl4q,int,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						0x2, 0x13, 0xf9, 0xf9,
						0xf9, 0xf9, 0xf9, 0xf9,
						0xf9, 0xf9, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbl4q,uint,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						 0x2, 0x13, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbl4q,poly,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						 0x2, 0x13, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9 };

/* Expected results for vqtbx1q.  */
VECT_VAR_DECL(expected_vqtbx1q,int,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						0x33, 0x33, 0xf3, 0xf3,
						0xf3, 0xf3, 0xf3, 0xf3,
						0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbx1q,uint,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						 0xcc, 0xcc, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vqtbx1q,poly,8,16) [] = { 0xfb, 0xf3, 0xf3, 0xf3,
						 0xcc, 0xcc, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3,
						 0xf3, 0xf3, 0xf3, 0xf3 };

/* Expected results for vqtbx2q.  */
VECT_VAR_DECL(expected_vqtbx2q,int,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						0xfa, 0x33, 0xf5, 0xf5,
						0xf5, 0xf5, 0xf5, 0xf5,
						0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbx2q,uint,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						 0xfa, 0xcc, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vqtbx2q,poly,8,16) [] = { 0x5, 0xf5, 0xf5, 0xf5,
						 0xfa, 0xcc, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5,
						 0xf5, 0xf5, 0xf5, 0xf5 };

/* Expected results for vqtbx3q.  */
VECT_VAR_DECL(expected_vqtbx3q,int,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						0xfe, 0xb, 0xf7, 0xf7,
						0xf7, 0xf7, 0xf7, 0xf7,
						0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbx3q,uint,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						 0xfe, 0xb, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vqtbx3q,poly,8,16) [] = { 0xf, 0xf7, 0xf7, 0xf7,
						 0xfe, 0xb, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7,
						 0xf7, 0xf7, 0xf7, 0xf7 };

/* Expected results for vqtbx4q.  */
VECT_VAR_DECL(expected_vqtbx4q,int,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						0x2, 0x13, 0xf9, 0xf9,
						0xf9, 0xf9, 0xf9, 0xf9,
						0xf9, 0xf9, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbx4q,uint,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						 0x2, 0x13, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vqtbx4q,poly,8,16) [] = { 0x19, 0xf9, 0xf9, 0xf9,
						 0x2, 0x13, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9,
						 0xf9, 0xf9, 0xf9, 0xf9 };

void exec_vqtbX (void)
{
  int i;

  /* In this case, input variables are arrays of vectors.  */
#define DECL_VQTBX(T1, W, N, X)						\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(table_vector, T1, W, N, X)

  /* The vqtbl1 variant is different from vqtbl{2,3,4} because it takes a
     vector as 1st param, instead of an array of vectors.  */
#define TEST_VQTBL1(T1, T2, T3, W, N1, N2)		\
  VECT_VAR(table_vector, T1, W, N2) =			\
    vld1##q_##T2##W((T1##W##_t *)lookup_table);		\
							\
  VECT_VAR(vector_res, T1, W, N1) =			\
    vqtbl1_##T2##W(VECT_VAR(table_vector, T1, W, N2),	\
		   VECT_VAR(vector, T3, W, N1));	\
  vst1_##T2##W(VECT_VAR(result, T1, W, N1),		\
	       VECT_VAR(vector_res, T1, W, N1));

#define TEST_VQTBL1Q(T1, T2, T3, W, N1, N2)		\
  VECT_VAR(table_vector, T1, W, N2) =			\
    vld1##q_##T2##W((T1##W##_t *)lookup_table);		\
							\
  VECT_VAR(vector_res, T1, W, N1) =			\
    vqtbl1q_##T2##W(VECT_VAR(table_vector, T1, W, N2),	\
		    VECT_VAR(vector, T3, W, N1));	\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N1),		\
	       VECT_VAR(vector_res, T1, W, N1));

#define TEST_VQTBLX(T1, T2, T3, W, N1, N2, X)				\
  VECT_ARRAY_VAR(table_vector, T1, W, N2, X) =				\
    vld##X##q_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N1) =					\
    vqtbl##X##_##T2##W(VECT_ARRAY_VAR(table_vector, T1, W, N2, X),	\
		       VECT_VAR(vector, T3, W, N1));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N1),				\
		VECT_VAR(vector_res, T1, W, N1));

#define TEST_VQTBLXQ(T1, T2, T3, W, N1, N2, X)				\
  VECT_ARRAY_VAR(table_vector, T1, W, N2, X) =				\
    vld##X##q_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N1) =					\
    vqtbl##X##q_##T2##W(VECT_ARRAY_VAR(table_vector, T1, W, N2, X),	\
			VECT_VAR(vector, T3, W, N1));			\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N1),				\
		VECT_VAR(vector_res, T1, W, N1));

  /* We need to define a lookup table large enough.  */
  int8_t lookup_table[4*16];

  /* For vqtblX.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, poly, 8, 8);

  /* For vqtblXq.  */
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, poly, 8, 16);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, poly, 8, 16);

  /* For vqtbl1.  */
  DECL_VARIABLE(table_vector, int, 8, 16);
  DECL_VARIABLE(table_vector, uint, 8, 16);
  DECL_VARIABLE(table_vector, poly, 8, 16);

  /* For vqtbx*.  */
  DECL_VARIABLE(default_vector, int, 8, 8);
  DECL_VARIABLE(default_vector, uint, 8, 8);
  DECL_VARIABLE(default_vector, poly, 8, 8);

  /* For vqtbx*q.  */
  DECL_VARIABLE(default_vector, int, 8, 16);
  DECL_VARIABLE(default_vector, uint, 8, 16);
  DECL_VARIABLE(default_vector, poly, 8, 16);

  /* We need only 8 bits variants.  */
#define DECL_ALL_VQTBLX(X)			\
  DECL_VQTBX(int, 8, 16, X);			\
  DECL_VQTBX(uint, 8, 16, X);			\
  DECL_VQTBX(poly, 8, 16, X)

#define TEST_ALL_VQTBL1()			\
  TEST_VQTBL1(int, s, uint, 8, 8, 16);		\
  TEST_VQTBL1(uint, u, uint, 8, 8, 16);		\
  TEST_VQTBL1(poly, p, uint, 8, 8, 16);		\
  TEST_VQTBL1Q(int, s, uint, 8, 16, 16);	\
  TEST_VQTBL1Q(uint, u, uint, 8, 16, 16);	\
  TEST_VQTBL1Q(poly, p, uint, 8, 16, 16)

#define TEST_ALL_VQTBLX(X)			\
  TEST_VQTBLX(int, s, uint, 8, 8, 16, X);	\
  TEST_VQTBLX(uint, u, uint, 8, 8, 16, X);	\
  TEST_VQTBLX(poly, p, uint, 8, 8, 16, X);	\
  TEST_VQTBLXQ(int, s, uint, 8, 16, 16, X);	\
  TEST_VQTBLXQ(uint, u, uint, 8, 16, 16, X);	\
  TEST_VQTBLXQ(poly, p, uint, 8, 16, 16, X)

  /* Declare the temporary buffers / variables.  */
  DECL_ALL_VQTBLX(2);
  DECL_ALL_VQTBLX(3);
  DECL_ALL_VQTBLX(4);

  /* Fill the lookup table.  */
  for (i=0; i<4*16; i++) {
    lookup_table[i] = i-15;
  }

  /* Choose init value arbitrarily, will be used as table index.  */
  VDUP(vector, , uint, u, 8, 8, 2);
  VDUP(vector, q, uint, u, 8, 16, 2);

  /* To ensure coverage, add some indexes larger than 8, 16 and 32
     except: lane 0 (index 10), lane 4 (index 20) and lane 5 (index
     40).  */
  VSET_LANE(vector, , uint, u, 8, 8, 0, 10);
  VSET_LANE(vector, , uint, u, 8, 8, 4, 20);
  VSET_LANE(vector, , uint, u, 8, 8, 5, 40);

  VSET_LANE(vector, q, uint, u, 8, 16, 0, 10);
  VSET_LANE(vector, q, uint, u, 8, 16, 4, 20);
  VSET_LANE(vector, q, uint, u, 8, 16, 5, 40);

  /* Check vqtbl1.  */
  clean_results ();
#define TEST_MSG "VQTBL1"
  TEST_ALL_VQTBL1();

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbl1, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbl1, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbl1, "");

#undef TEST_MSG
#define TEST_MSG "VQTBL1Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbl1q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbl1q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbl1q, "");

  /* Check vqtbl2.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBL2"
  TEST_ALL_VQTBLX(2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbl2, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbl2, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbl2, "");

#undef TEST_MSG
#define TEST_MSG "VQTBL2Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbl2q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbl2q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbl2q, "");

  /* Check vqtbl3.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBL3"
  TEST_ALL_VQTBLX(3);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbl3, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbl3, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbl3, "");

#undef TEST_MSG
#define TEST_MSG "VQTBL3Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbl3q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbl3q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbl3q, "");

  /* Check vqtbl4.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBL4"
  TEST_ALL_VQTBLX(4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbl4, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbl4, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbl4, "");

#undef TEST_MSG
#define TEST_MSG "VQTBL4Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbl4q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbl4q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbl4q, "");


  /* Now test VQTBX.  */

  /* The vqtbx1 variant is different from vqtbx{2,3,4} because it takes a
     vector as 1st param, instead of an array of vectors.  */
#define TEST_VQTBX1(T1, T2, T3, W, N1, N2)		\
  VECT_VAR(table_vector, T1, W, N2) =			\
    vld1##q_##T2##W((T1##W##_t *)lookup_table);		\
    							\
  VECT_VAR(vector_res, T1, W, N1) =		       	\
    vqtbx1_##T2##W(VECT_VAR(default_vector, T1, W, N1),	\
		   VECT_VAR(table_vector, T1, W, N2),	\
		   VECT_VAR(vector, T3, W, N1));	\
  vst1_##T2##W(VECT_VAR(result, T1, W, N1),		\
	       VECT_VAR(vector_res, T1, W, N1));

#define TEST_VQTBX1Q(T1, T2, T3, W, N1, N2)		\
  VECT_VAR(table_vector, T1, W, N2) =			\
    vld1##q_##T2##W((T1##W##_t *)lookup_table);		\
    							\
  VECT_VAR(vector_res, T1, W, N1) =			\
    vqtbx1q_##T2##W(VECT_VAR(default_vector, T1, W, N1),\
		    VECT_VAR(table_vector, T1, W, N2),	\
		    VECT_VAR(vector, T3, W, N1));	\
    vst1q_##T2##W(VECT_VAR(result, T1, W, N1),		\
		  VECT_VAR(vector_res, T1, W, N1));
  
#define TEST_VQTBXX(T1, T2, T3, W, N1, N2, X)				\
  VECT_ARRAY_VAR(table_vector, T1, W, N2, X) =				\
    vld##X##q_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N1) =					\
    vqtbx##X##_##T2##W(VECT_VAR(default_vector, T1, W, N1),		\
			VECT_ARRAY_VAR(table_vector, T1, W, N2, X),	\
			VECT_VAR(vector, T3, W, N1));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N1),				\
		VECT_VAR(vector_res, T1, W, N1));

#define TEST_VQTBXXQ(T1, T2, T3, W, N1, N2, X)				\
  VECT_ARRAY_VAR(table_vector, T1, W, N2, X) =				\
    vld##X##q_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N1) =					\
    vqtbx##X##q_##T2##W(VECT_VAR(default_vector, T1, W, N1),		\
			VECT_ARRAY_VAR(table_vector, T1, W, N2, X),	\
			VECT_VAR(vector, T3, W, N1));			\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N1),				\
		VECT_VAR(vector_res, T1, W, N1));

#define TEST_ALL_VQTBX1()			\
  TEST_VQTBX1(int, s, uint, 8, 8, 16);		\
  TEST_VQTBX1(uint, u, uint, 8, 8, 16);		\
  TEST_VQTBX1(poly, p, uint, 8, 8, 16);		\
  TEST_VQTBX1Q(int, s, uint, 8, 16, 16);	\
  TEST_VQTBX1Q(uint, u, uint, 8, 16, 16);	\
  TEST_VQTBX1Q(poly, p, uint, 8, 16, 16)

#define TEST_ALL_VQTBXX(X)			\
  TEST_VQTBXX(int, s, uint, 8, 8, 16, X);	\
  TEST_VQTBXX(uint, u, uint, 8, 8, 16, X);	\
  TEST_VQTBXX(poly, p, uint, 8, 8, 16, X);	\
  TEST_VQTBXXQ(int, s, uint, 8, 16, 16, X);	\
  TEST_VQTBXXQ(uint, u, uint, 8, 16, 16, X);	\
  TEST_VQTBXXQ(poly, p, uint, 8, 16, 16, X)

  /* Choose init value arbitrarily, will be used as default value.  */
  VDUP(default_vector, , int, s, 8, 8, 0x33);
  VDUP(default_vector, , uint, u, 8, 8, 0xCC);
  VDUP(default_vector, , poly, p, 8, 8, 0xCC);
  VDUP(default_vector, q, int, s, 8, 16, 0x33);
  VDUP(default_vector, q, uint, u, 8, 16, 0xCC);
  VDUP(default_vector, q, poly, p, 8, 16, 0xCC);

  /* Check vqtbx1.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBX1"
  TEST_ALL_VQTBX1();

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbx1, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbx1, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbx1, "");

#undef TEST_MSG
#define TEST_MSG "VQTBX1Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbx1q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbx1q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbx1q, "");

  /* Check vqtbx2.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBX2"
  TEST_ALL_VQTBXX(2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbx2, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbx2, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbx2, "");

#undef TEST_MSG
#define TEST_MSG "VQTBX2Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbx2q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbx2q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbx2q, "");

  /* Check vqtbx3.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBX3"
  TEST_ALL_VQTBXX(3);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbx3, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbx3, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbx3, "");

#undef TEST_MSG
#define TEST_MSG "VQTBX3Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbx3q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbx3q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbx3q, "");

  /* Check vqtbx4.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VQTBX4"
  TEST_ALL_VQTBXX(4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vqtbx4, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vqtbx4, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vqtbx4, "");

#undef TEST_MSG
#define TEST_MSG "VQTBX4Q"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_vqtbx4q, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_vqtbx4q, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_vqtbx4q, "");
}

int main (void)
{
  exec_vqtbX ();
  return 0;
}
