#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results for vtbl1.  */
VECT_VAR_DECL(expected_vtbl1,int,8,8) [] = { 0x0, 0xf2, 0xf2, 0xf2,
					     0x0, 0x0, 0xf2, 0xf2 };
VECT_VAR_DECL(expected_vtbl1,uint,8,8) [] = { 0x0, 0xf3, 0xf3, 0xf3,
					      0x0, 0x0, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vtbl1,poly,8,8) [] = { 0x0, 0xf3, 0xf3, 0xf3,
					      0x0, 0x0, 0xf3, 0xf3 };

/* Expected results for vtbl2.  */
VECT_VAR_DECL(expected_vtbl2,int,8,8) [] = { 0xf6, 0xf3, 0xf3, 0xf3,
					     0x0, 0x0, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vtbl2,uint,8,8) [] = { 0xf6, 0xf5, 0xf5, 0xf5,
					      0x0, 0x0, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vtbl2,poly,8,8) [] = { 0xf6, 0xf5, 0xf5, 0xf5,
					      0x0, 0x0, 0xf5, 0xf5 };

/* Expected results for vtbl3.  */
VECT_VAR_DECL(expected_vtbl3,int,8,8) [] = { 0xf8, 0xf4, 0xf4, 0xf4,
					     0xff, 0x0, 0xf4, 0xf4 };
VECT_VAR_DECL(expected_vtbl3,uint,8,8) [] = { 0xf8, 0xf7, 0xf7, 0xf7,
					      0xff, 0x0, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vtbl3,poly,8,8) [] = { 0xf8, 0xf7, 0xf7, 0xf7,
					      0xff, 0x0, 0xf7, 0xf7 };

/* Expected results for vtbl4.  */
VECT_VAR_DECL(expected_vtbl4,int,8,8) [] = { 0xfa, 0xf5, 0xf5, 0xf5,
					    0x3, 0x0, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vtbl4,uint,8,8) [] = { 0xfa, 0xf9, 0xf9, 0xf9,
					     0x3, 0x0, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vtbl4,poly,8,8) [] = { 0xfa, 0xf9, 0xf9, 0xf9,
					     0x3, 0x0, 0xf9, 0xf9 };

/* Expected results for vtbx1.  */
VECT_VAR_DECL(expected_vtbx1,int,8,8) [] = { 0x33, 0xf2, 0xf2, 0xf2,
					    0x33, 0x33, 0xf2, 0xf2 };
VECT_VAR_DECL(expected_vtbx1,uint,8,8) [] = { 0xcc, 0xf3, 0xf3, 0xf3,
					     0xcc, 0xcc, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vtbx1,poly,8,8) [] = { 0xcc, 0xf3, 0xf3, 0xf3,
					     0xcc, 0xcc, 0xf3, 0xf3 };

/* Expected results for vtbx2.  */
VECT_VAR_DECL(expected_vtbx2,int,8,8) [] = { 0xf6, 0xf3, 0xf3, 0xf3,
					    0x33, 0x33, 0xf3, 0xf3 };
VECT_VAR_DECL(expected_vtbx2,uint,8,8) [] = { 0xf6, 0xf5, 0xf5, 0xf5,
					     0xcc, 0xcc, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vtbx2,poly,8,8) [] = { 0xf6, 0xf5, 0xf5, 0xf5,
					     0xcc, 0xcc, 0xf5, 0xf5 };

/* Expected results for vtbx3.  */
VECT_VAR_DECL(expected_vtbx3,int,8,8) [] = { 0xf8, 0xf4, 0xf4, 0xf4,
					    0xff, 0x33, 0xf4, 0xf4 };
VECT_VAR_DECL(expected_vtbx3,uint,8,8) [] = { 0xf8, 0xf7, 0xf7, 0xf7,
					     0xff, 0xcc, 0xf7, 0xf7 };
VECT_VAR_DECL(expected_vtbx3,poly,8,8) [] = { 0xf8, 0xf7, 0xf7, 0xf7,
					     0xff, 0xcc, 0xf7, 0xf7 };

/* Expected results for vtbx4.  */
VECT_VAR_DECL(expected_vtbx4,int,8,8) [] = { 0xfa, 0xf5, 0xf5, 0xf5,
					     0x3, 0x33, 0xf5, 0xf5 };
VECT_VAR_DECL(expected_vtbx4,uint,8,8) [] = { 0xfa, 0xf9, 0xf9, 0xf9,
					      0x3, 0xcc, 0xf9, 0xf9 };
VECT_VAR_DECL(expected_vtbx4,poly,8,8) [] = { 0xfa, 0xf9, 0xf9, 0xf9,
					      0x3, 0xcc, 0xf9, 0xf9 };

void exec_vtbX (void)
{
  int i;

  /* In this case, input variables are arrays of vectors.  */
#define DECL_VTBX(T1, W, N, X)						\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(table_vector, T1, W, N, X)

  /* The vtbl1 variant is different from vtbl{2,3,4} because it takes a
     vector as 1st param, instead of an array of vectors.  */
#define TEST_VTBL1(T1, T2, T3, W, N)			\
  VECT_VAR(table_vector, T1, W, N) =			\
    vld1##_##T2##W((T1##W##_t *)lookup_table);		\
							\
  VECT_VAR(vector_res, T1, W, N) =			\
    vtbl1_##T2##W(VECT_VAR(table_vector, T1, W, N),	\
		  VECT_VAR(vector, T3, W, N));		\
  vst1_##T2##W(VECT_VAR(result, T1, W, N),		\
	       VECT_VAR(vector_res, T1, W, N));

#define TEST_VTBLX(T1, T2, T3, W, N, X)					\
  VECT_ARRAY_VAR(table_vector, T1, W, N, X) =				\
    vld##X##_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N) =					\
    vtbl##X##_##T2##W(VECT_ARRAY_VAR(table_vector, T1, W, N, X),	\
		      VECT_VAR(vector, T3, W, N));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N),				\
	       VECT_VAR(vector_res, T1, W, N));

  /* We need to define a lookup table.  */
  uint8_t lookup_table[32];

  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, poly, 8, 8);

  /* For vtbl1.  */
  DECL_VARIABLE(table_vector, int, 8, 8);
  DECL_VARIABLE(table_vector, uint, 8, 8);
  DECL_VARIABLE(table_vector, poly, 8, 8);

  /* For vtbx*.  */
  DECL_VARIABLE(default_vector, int, 8, 8);
  DECL_VARIABLE(default_vector, uint, 8, 8);
  DECL_VARIABLE(default_vector, poly, 8, 8);

  /* We need only 8 bits variants.  */
#define DECL_ALL_VTBLX(X)			\
  DECL_VTBX(int, 8, 8, X);			\
  DECL_VTBX(uint, 8, 8, X);			\
  DECL_VTBX(poly, 8, 8, X)

#define TEST_ALL_VTBL1()			\
  TEST_VTBL1(int, s, int, 8, 8);		\
  TEST_VTBL1(uint, u, uint, 8, 8);		\
  TEST_VTBL1(poly, p, uint, 8, 8)

#define TEST_ALL_VTBLX(X)			\
  TEST_VTBLX(int, s, int, 8, 8, X);		\
  TEST_VTBLX(uint, u, uint, 8, 8, X);		\
  TEST_VTBLX(poly, p, uint, 8, 8, X)

  /* Declare the temporary buffers / variables.  */
  DECL_ALL_VTBLX(2);
  DECL_ALL_VTBLX(3);
  DECL_ALL_VTBLX(4);

  /* Fill the lookup table.  */
  for (i=0; i<32; i++) {
    lookup_table[i] = i-15;
  }

  /* Choose init value arbitrarily, will be used as table index.  */
  VDUP(vector, , int, s, 8, 8, 1);
  VDUP(vector, , uint, u, 8, 8, 2);
  VDUP(vector, , poly, p, 8, 8, 2);

  /* To ensure coverage, add some indexes larger than 8, 16 and 32
     except: lane 0 (by 10), lane 4 (by 20) and lane 5 (by 40).  */
  VSET_LANE(vector, , int, s, 8, 8, 0, 10);
  VSET_LANE(vector, , int, s, 8, 8, 4, 20);
  VSET_LANE(vector, , int, s, 8, 8, 5, 40);
  VSET_LANE(vector, , uint, u, 8, 8, 0, 10);
  VSET_LANE(vector, , uint, u, 8, 8, 4, 20);
  VSET_LANE(vector, , uint, u, 8, 8, 5, 40);
  VSET_LANE(vector, , poly, p, 8, 8, 0, 10);
  VSET_LANE(vector, , poly, p, 8, 8, 4, 20);
  VSET_LANE(vector, , poly, p, 8, 8, 5, 40);


  /* Check vtbl1.  */
  clean_results ();
#define TEST_MSG "VTBL1"
  TEST_ALL_VTBL1();

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbl1, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbl1, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbl1, "");

  /* Check vtbl2.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBL2"
  TEST_ALL_VTBLX(2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbl2, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbl2, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbl2, "");

  /* Check vtbl3.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBL3"
  TEST_ALL_VTBLX(3);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbl3, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbl3, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbl3, "");

  /* Check vtbl4.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBL4"
  TEST_ALL_VTBLX(4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbl4, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbl4, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbl4, "");


  /* Now test VTBX.  */

  /* The vtbx1 variant is different from vtbx{2,3,4} because it takes a
     vector as 1st param, instead of an array of vectors.  */
#define TEST_VTBX1(T1, T2, T3, W, N)			\
  VECT_VAR(table_vector, T1, W, N) =			\
    vld1##_##T2##W((T1##W##_t *)lookup_table);		\
							\
  VECT_VAR(vector_res, T1, W, N) =			\
    vtbx1_##T2##W(VECT_VAR(default_vector, T1, W, N),	\
		  VECT_VAR(table_vector, T1, W, N),	\
		  VECT_VAR(vector, T3, W, N));		\
  vst1_##T2##W(VECT_VAR(result, T1, W, N),		\
	       VECT_VAR(vector_res, T1, W, N));

#define TEST_VTBXX(T1, T2, T3, W, N, X)					\
  VECT_ARRAY_VAR(table_vector, T1, W, N, X) =				\
    vld##X##_##T2##W((T1##W##_t *)lookup_table);			\
									\
  VECT_VAR(vector_res, T1, W, N) =					\
    vtbx##X##_##T2##W(VECT_VAR(default_vector, T1, W, N),		\
		      VECT_ARRAY_VAR(table_vector, T1, W, N, X),	\
		      VECT_VAR(vector, T3, W, N));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N),				\
	       VECT_VAR(vector_res, T1, W, N));

#define TEST_ALL_VTBX1()			\
  TEST_VTBX1(int, s, int, 8, 8);		\
  TEST_VTBX1(uint, u, uint, 8, 8);		\
  TEST_VTBX1(poly, p, uint, 8, 8)

#define TEST_ALL_VTBXX(X)			\
  TEST_VTBXX(int, s, int, 8, 8, X);		\
  TEST_VTBXX(uint, u, uint, 8, 8, X);		\
  TEST_VTBXX(poly, p, uint, 8, 8, X)

  /* Choose init value arbitrarily, will be used as default value.  */
  VDUP(default_vector, , int, s, 8, 8, 0x33);
  VDUP(default_vector, , uint, u, 8, 8, 0xCC);
  VDUP(default_vector, , poly, p, 8, 8, 0xCC);

  /* Check vtbx1.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBX1"
  TEST_ALL_VTBX1();

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbx1, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbx1, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbx1, "");

  /* Check vtbx2.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBX2"
  TEST_ALL_VTBXX(2);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbx2, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbx2, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbx2, "");

  /* Check vtbx3.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBX3"
  TEST_ALL_VTBXX(3);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbx3, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbx3, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbx3, "");

  /* Check vtbx4.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VTBX4"
  TEST_ALL_VTBXX(4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_vtbx4, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_vtbx4, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_vtbx4, "");
}

int main (void)
{
  exec_vtbX ();
  return 0;
}
