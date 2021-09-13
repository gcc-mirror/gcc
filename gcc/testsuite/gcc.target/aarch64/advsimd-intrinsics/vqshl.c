#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"


/* Expected results with input=0.  */
VECT_VAR_DECL(expected_0,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					   0x0, 0x0, 0x0, 0x0,
					   0x0, 0x0, 0x0, 0x0,
					   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,64,2) [] = { 0x0, 0x0 };

/* Expected results with input=0 and negative shift amount.  */
VECT_VAR_DECL(expected_0_neg,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0_neg,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_neg,uint,64,2) [] = { 0x0, 0x0 };

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xe0, 0xe2, 0xe4, 0xe6,
				       0xe8, 0xea, 0xec, 0xee };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xff80, 0xff88, 0xff90, 0xff98 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffff000, 0xfffff100 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffffe };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x1ffffffffffffffe };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x8000, 0x8000, 0x8000, 0x8000,
					0x8000, 0x8000, 0x8000, 0x8000 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x80000000, 0x80000000,
					0x80000000, 0x80000000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x8000000000000000,
					0x8000000000000000 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffff, 0xffffffff,
					 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffffff,
					 0xffffffffffffffff };

/* Expected results with negative shift amount.  */
VECT_VAR_DECL(expected_neg,int,8,8) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
					   0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected_neg,int,16,4) [] = { 0xfffc, 0xfffc, 0xfffc, 0xfffc };
VECT_VAR_DECL(expected_neg,int,32,2) [] = { 0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected_neg,int,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x78, 0x78, 0x79, 0x79,
					    0x7a, 0x7a, 0x7b, 0x7b };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x3ffc, 0x3ffc, 0x3ffc, 0x3ffc };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x1ffffffe, 0x1ffffffe };
VECT_VAR_DECL(expected_neg,uint,64,1) [] = { 0xfffffffffffffff };
VECT_VAR_DECL(expected_neg,int,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_neg,int,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					    0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_neg,int,32,4) [] = { 0xffffffff, 0xffffffff,
					    0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_neg,int,64,2) [] = { 0xffffffffffffffff,
					    0xffffffffffffffff };
VECT_VAR_DECL(expected_neg,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
					     0x1, 0x1, 0x1, 0x1,
					     0x1, 0x1, 0x1, 0x1,
					     0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_neg,uint,16,8) [] = { 0x1f, 0x1f, 0x1f, 0x1f,
					     0x1f, 0x1f, 0x1f, 0x1f };
VECT_VAR_DECL(expected_neg,uint,32,4) [] = { 0x7ffff, 0x7ffff,
					     0x7ffff, 0x7ffff };
VECT_VAR_DECL(expected_neg,uint,64,2) [] = { 0xfffffffffff, 0xfffffffffff };

/* Expected results with negative input and large shift amount.  */
VECT_VAR_DECL(expected_neg_large,int,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						 0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_neg_large,int,16,4) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL(expected_neg_large,int,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_neg_large,int,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_neg_large,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_neg_large,uint,16,4) [] = { 0xffff, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL(expected_neg_large,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_neg_large,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_neg_large,int,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_neg_large,int,16,8) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL(expected_neg_large,int,32,4) [] = { 0x80000000, 0x80000000,
						  0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_neg_large,int,64,2) [] = { 0x8000000000000000,
						  0x8000000000000000 };
VECT_VAR_DECL(expected_neg_large,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_neg_large,uint,16,8) [] = { 0xffff, 0xffff,
						   0xffff, 0xffff,
						   0xffff, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL(expected_neg_large,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						   0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_neg_large,uint,64,2) [] = { 0xffffffffffffffff,
						   0xffffffffffffffff };

/* Expected results with max input and shift by -1.  */
VECT_VAR_DECL(expected_max_minus1,int,8,8) [] = { 0x3f, 0x3f, 0x3f, 0x3f,
						  0x3f, 0x3f, 0x3f, 0x3f };
VECT_VAR_DECL(expected_max_minus1,int,16,4) [] = { 0x3fff, 0x3fff,
						   0x3fff, 0x3fff };
VECT_VAR_DECL(expected_max_minus1,int,32,2) [] = { 0x3fffffff, 0x3fffffff };
VECT_VAR_DECL(expected_max_minus1,int,64,1) [] = { 0x3fffffffffffffff };
VECT_VAR_DECL(expected_max_minus1,uint,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						   0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_minus1,uint,16,4) [] = { 0x7fff, 0x7fff,
						    0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_minus1,uint,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_minus1,uint,64,1) [] = { 0x7fffffffffffffff };
VECT_VAR_DECL(expected_max_minus1,int,8,16) [] = { 0x3f, 0x3f, 0x3f, 0x3f,
						   0x3f, 0x3f, 0x3f, 0x3f,
						   0x3f, 0x3f, 0x3f, 0x3f,
						   0x3f, 0x3f, 0x3f, 0x3f };
VECT_VAR_DECL(expected_max_minus1,int,16,8) [] = { 0x3fff, 0x3fff,
						   0x3fff, 0x3fff,
						   0x3fff, 0x3fff,
						   0x3fff, 0x3fff };
VECT_VAR_DECL(expected_max_minus1,int,32,4) [] = { 0x3fffffff, 0x3fffffff,
						   0x3fffffff, 0x3fffffff };
VECT_VAR_DECL(expected_max_minus1,int,64,2) [] = { 0x3fffffffffffffff,
						   0x3fffffffffffffff };
VECT_VAR_DECL(expected_max_minus1,uint,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						    0x7f, 0x7f, 0x7f, 0x7f,
						    0x7f, 0x7f, 0x7f, 0x7f,
						    0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_minus1,uint,16,8) [] = { 0x7fff, 0x7fff,
						    0x7fff, 0x7fff,
						    0x7fff, 0x7fff,
						    0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_minus1,uint,32,4) [] = { 0x7fffffff, 0x7fffffff,
						    0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_minus1,uint,64,2) [] = { 0x7fffffffffffffff,
						    0x7fffffffffffffff };

/* Expected results with max input and large shift amount.  */
VECT_VAR_DECL(expected_max_large,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					       0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_large,int,16,4) [] = { 0x7fff, 0x7fff,
						0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_large,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_large,int,64,1) [] = { 0x7fffffffffffffff };
VECT_VAR_DECL(expected_max_large,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_large,uint,16,4) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_large,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max_large,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_max_large,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_large,int,16,8) [] = { 0x7fff, 0x7fff,
						0x7fff, 0x7fff,
						0x7fff, 0x7fff,
						0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_large,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
						0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_large,int,64,2) [] = { 0x7fffffffffffffff,
						0x7fffffffffffffff };
VECT_VAR_DECL(expected_max_large,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_large,uint,16,8) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff,
						 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_large,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max_large,uint,64,2) [] = { 0xffffffffffffffff,
						 0xffffffffffffffff };

/* Expected results with saturation on 64-bits values..  */
VECT_VAR_DECL(expected_64,int,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_64,int,64,2) [] = { 0x7fffffffffffffff,
					   0x7fffffffffffffff };

#define INSN vqshl
#define TEST_MSG "VQSHL/VQSHLQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: v3=vqshl(v1,v2), then store the result.  */
#define TEST_VQSHL2(INSN, T3, Q, T1, T2, W, N, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector_shift, T3, W, N));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHL1(INSN, T3, Q, T1, T2, W, N, CMT) \
  TEST_VQSHL2(INSN, T3, Q, T1, T2, W, N, CMT)

#define TEST_VQSHL(T3, Q, T1, T2, W, N, CMT)	\
  TEST_VQSHL1(INSN, T3, Q, T1, T2, W, N, CMT)


  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  DECL_VARIABLE_SIGNED_VARIANTS(vector_shift);

  clean_results ();

  /* Fill input vector with 0, to check saturation on limits.  */
  VDUP(vector, , int, s, 8, 8, 0);
  VDUP(vector, , int, s, 16, 4, 0);
  VDUP(vector, , int, s, 32, 2, 0);
  VDUP(vector, , int, s, 64, 1, 0);
  VDUP(vector, , uint, u, 8, 8, 0);
  VDUP(vector, , uint, u, 16, 4, 0);
  VDUP(vector, , uint, u, 32, 2, 0);
  VDUP(vector, , uint, u, 64, 1, 0);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 8, 16, 0);
  VDUP(vector, q, uint, u, 16, 8, 0);
  VDUP(vector, q, uint, u, 32, 4, 0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Choose init value arbitrarily, will be used as shift amount */
  /* Use values equal or one-less-than the type width to check
     behavior on limits.  */

  /* 64-bits vectors first.  */
  /* Shift 8-bits lanes by 7...  */
  VDUP(vector_shift, , int, s, 8, 8, 7);
  /* ... except: lane 0 (by 6), lane 1 (by 8) and lane 2 (by 9).  */
  VSET_LANE(vector_shift, , int, s, 8, 8, 0, 6);
  VSET_LANE(vector_shift, , int, s, 8, 8, 1, 8);
  VSET_LANE(vector_shift, , int, s, 8, 8, 2, 9);

  /* Shift 16-bits lanes by 15... */
  VDUP(vector_shift, , int, s, 16, 4, 15);
  /* ... except: lane 0 (by 14), lane 1 (by 16), and lane 2 (by 17).  */
  VSET_LANE(vector_shift, , int, s, 16, 4, 0, 14);
  VSET_LANE(vector_shift, , int, s, 16, 4, 1, 16);
  VSET_LANE(vector_shift, , int, s, 16, 4, 2, 17);

  /* Shift 32-bits lanes by 31... */
  VDUP(vector_shift, , int, s, 32, 2, 31);
  /* ... except lane 1 (by 30).  */
  VSET_LANE(vector_shift, , int, s, 32, 2, 1, 30);

  /* Shift 64 bits lane by 63.  */
  VDUP(vector_shift, , int, s, 64, 1, 63);

  /* 128-bits vectors.  */
  /* Shift 8-bits lanes by 8.  */
  VDUP(vector_shift, q, int, s, 8, 16, 8);
  /* Shift 16-bits lanes by 16.  */
  VDUP(vector_shift, q, int, s, 16, 8, 16);
  /* Shift 32-bits lanes by 32...  */
  VDUP(vector_shift, q, int, s, 32, 4, 32);
  /* ... except lane 1 (by 33).  */
  VSET_LANE(vector_shift, q, int, s, 32, 4, 1, 33);

  /* Shift 64-bits lanes by 64... */
  VDUP(vector_shift, q, int, s, 64, 2, 64);
  /* ... except lane 1 (by 62).  */
  VSET_LANE(vector_shift, q, int, s, 64, 2, 1, 62);

#define CMT " (with input = 0)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_0, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_0, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_0, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_0, CMT);


  /* Use negative shift amounts */
  VDUP(vector_shift, , int, s, 8, 8, -1);
  VDUP(vector_shift, , int, s, 16, 4, -2);
  VDUP(vector_shift, , int, s, 32, 2, -3);
  VDUP(vector_shift, , int, s, 64, 1, -4);
  VDUP(vector_shift, q, int, s, 8, 16, -7);
  VDUP(vector_shift, q, int, s, 16, 8, -11);
  VDUP(vector_shift, q, int, s, 32, 4, -13);
  VDUP(vector_shift, q, int, s, 64, 2, -20);

#undef CMT
#define CMT " (input 0 and negative shift amount)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_0_neg, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_0_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_0_neg, CMT);

  /* Test again, with predefined input values.  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose init value arbitrarily, will be used as shift amount.  */
  VDUP(vector_shift, , int, s, 8, 8, 1);
  VDUP(vector_shift, , int, s, 16, 4, 3);
  VDUP(vector_shift, , int, s, 32, 2, 8);
  VDUP(vector_shift, , int, s, 64, 1, -3);
  VDUP(vector_shift, q, int, s, 8, 16, 10);
  VDUP(vector_shift, q, int, s, 16, 8, 12);
  VDUP(vector_shift, q, int, s, 32, 4, 32);
  VDUP(vector_shift, q, int, s, 64, 2, 63);

#undef CMT
#define CMT ""
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, CMT);


  /* Use negative shift amounts */
  VDUP(vector_shift, , int, s, 8, 8, -1);
  VDUP(vector_shift, , int, s, 16, 4, -2);
  VDUP(vector_shift, , int, s, 32, 2, -3);
  VDUP(vector_shift, , int, s, 64, 1, -4);
  VDUP(vector_shift, q, int, s, 8, 16, -7);
  VDUP(vector_shift, q, int, s, 16, 8, -11);
  VDUP(vector_shift, q, int, s, 32, 4, -13);
  VDUP(vector_shift, q, int, s, 64, 2, -20);

#undef CMT
#define CMT " (negative shift amount)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_neg, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_neg, CMT);


  /* Use large shift amounts.  */
  VDUP(vector_shift, , int, s, 8, 8, 8);
  VDUP(vector_shift, , int, s, 16, 4, 16);
  VDUP(vector_shift, , int, s, 32, 2, 32);
  VDUP(vector_shift, , int, s, 64, 1, 64);
  VDUP(vector_shift, q, int, s, 8, 16, 8);
  VDUP(vector_shift, q, int, s, 16, 8, 16);
  VDUP(vector_shift, q, int, s, 32, 4, 32);
  VDUP(vector_shift, q, int, s, 64, 2, 64);

#undef CMT
#define CMT " (large shift amount, negative input)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_neg_large, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg_large, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_neg_large, CMT);


  /* Fill input vector with max value, to check saturation on limits */
  VDUP(vector, , int, s, 8, 8, 0x7F);
  VDUP(vector, , int, s, 16, 4, 0x7FFF);
  VDUP(vector, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, , uint, u, 8, 8, 0xFF);
  VDUP(vector, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, , uint, u, 64, 1, 0xFFFFFFFFFFFFFFFFULL);
  VDUP(vector, q, int, s, 8, 16, 0x7F);
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, uint, u, 8, 16, 0xFF);
  VDUP(vector, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

  /* Shift by -1 */
  VDUP(vector_shift, , int, s, 8, 8, -1);
  VDUP(vector_shift, , int, s, 16, 4, -1);
  VDUP(vector_shift, , int, s, 32, 2, -1);
  VDUP(vector_shift, , int, s, 64, 1, -1);
  VDUP(vector_shift, q, int, s, 8, 16, -1);
  VDUP(vector_shift, q, int, s, 16, 8, -1);
  VDUP(vector_shift, q, int, s, 32, 4, -1);
  VDUP(vector_shift, q, int, s, 64, 2, -1);

#undef CMT
#define CMT " (max input, shift by -1)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_minus1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_minus1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_minus1, CMT);


  /* Use large shift amounts */
  VDUP(vector_shift, , int, s, 8, 8, 8);
  VDUP(vector_shift, , int, s, 16, 4, 16);
  VDUP(vector_shift, , int, s, 32, 2, 32);
  VDUP(vector_shift, , int, s, 64, 1, 64);
  VDUP(vector_shift, q, int, s, 8, 16, 8);
  VDUP(vector_shift, q, int, s, 16, 8, 16);
  VDUP(vector_shift, q, int, s, 32, 4, 32);
  VDUP(vector_shift, q, int, s, 64, 2, 64);

#undef CMT
#define CMT " (max input, large shift amount)"
  TEST_VQSHL(int, , int, s, 8, 8, CMT);
  TEST_VQSHL(int, , int, s, 16, 4, CMT);
  TEST_VQSHL(int, , int, s, 32, 2, CMT);
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_large, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_large, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_large, CMT);


  /* Check 64 bits saturation.  */
  VDUP(vector, , int, s, 64, 1, -10);
  VDUP(vector_shift, , int, s, 64, 1, 64);
  VDUP(vector, q, int, s, 64, 2, 10);
  VDUP(vector_shift, q, int, s, 64, 2, 64);

#undef CMT
#define CMT " (check saturation on 64 bits)"
  TEST_VQSHL(int, , int, s, 64, 1, CMT);
  TEST_VQSHL(int, q, int, s, 64, 2, CMT);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64, CMT);
}

int main (void)
{
  exec_vqshl ();
  return 0;
}
