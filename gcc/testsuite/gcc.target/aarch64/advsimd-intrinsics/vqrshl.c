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
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffff80 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80,
					0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x8000, 0x8000, 0x8000, 0x8000,
					0x8000, 0x8000, 0x8000, 0x8000 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x80000000, 0x80000000,
					0x80000000, 0x80000000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x8000000000000000, 0x8000000000000000 };
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
VECT_VAR_DECL(expected_neg,int,8,8) [] = { 0xfc, 0xfc, 0xfd, 0xfd,
					   0xfd, 0xfd, 0xfe, 0xfe };
VECT_VAR_DECL(expected_neg,int,16,4) [] = { 0xfffc, 0xfffc, 0xfffd, 0xfffd };
VECT_VAR_DECL(expected_neg,int,32,2) [] = { 0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected_neg,int,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_neg,uint,8,8) [] = { 0x3c, 0x3c, 0x3d, 0x3d,
					    0x3d, 0x3d, 0x3e, 0x3e };
VECT_VAR_DECL(expected_neg,uint,16,4) [] = { 0x3ffc, 0x3ffc, 0x3ffd, 0x3ffd };
VECT_VAR_DECL(expected_neg,uint,32,2) [] = { 0x1ffffffe, 0x1ffffffe };
VECT_VAR_DECL(expected_neg,uint,64,1) [] = { 0xfffffffffffffff };
VECT_VAR_DECL(expected_neg,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_neg,uint,8,16) [] = { 0x2, 0x2, 0x2, 0x2,
					     0x2, 0x2, 0x2, 0x2,
					     0x2, 0x2, 0x2, 0x2,
					     0x2, 0x2, 0x2, 0x2 };
VECT_VAR_DECL(expected_neg,uint,16,8) [] = { 0x20, 0x20, 0x20, 0x20,
					     0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_neg,uint,32,4) [] = { 0x80000, 0x80000,
					     0x80000, 0x80000 };
VECT_VAR_DECL(expected_neg,uint,64,2) [] = { 0x100000000000, 0x100000000000 };

/* Expected results with input=max and shift by -1.  */
VECT_VAR_DECL(expected_minus1,int,8,8) [] = { 0x40, 0x40, 0x40, 0x40,
					      0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_minus1,int,16,4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected_minus1,int,32,2) [] = { 0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_minus1,int,64,1) [] = { 0x4000000000000000 };
VECT_VAR_DECL(expected_minus1,uint,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
					       0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_minus1,uint,16,4) [] = { 0x8000, 0x8000, 0x8000, 0x8000 };
VECT_VAR_DECL(expected_minus1,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_minus1,uint,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_minus1,int,8,16) [] = { 0x40, 0x40, 0x40, 0x40,
					       0x40, 0x40, 0x40, 0x40,
					       0x40, 0x40, 0x40, 0x40,
					       0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_minus1,int,16,8) [] = { 0x4000, 0x4000, 0x4000, 0x4000,
					       0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected_minus1,int,32,4) [] = { 0x40000000, 0x40000000,
					       0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_minus1,int,64,2) [] = { 0x4000000000000000,
					       0x4000000000000000 };
VECT_VAR_DECL(expected_minus1,uint,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_minus1,uint,16,8) [] = { 0x8000, 0x8000, 0x8000, 0x8000,
						0x8000, 0x8000, 0x8000, 0x8000 };
VECT_VAR_DECL(expected_minus1,uint,32,4) [] = { 0x80000000, 0x80000000,
						0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_minus1,uint,64,2) [] = { 0x8000000000000000,
						0x8000000000000000 };

/* Expected results with input=max and shift by -3.  */
VECT_VAR_DECL(expected_minus3,int,8,8) [] = { 0x10, 0x10, 0x10, 0x10,
					      0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_minus3,int,16,4) [] = { 0x1000, 0x1000, 0x1000, 0x1000 };
VECT_VAR_DECL(expected_minus3,int,32,2) [] = { 0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_minus3,int,64,1) [] = { 0x1000000000000000 };
VECT_VAR_DECL(expected_minus3,uint,8,8) [] = { 0x20, 0x20, 0x20, 0x20,
					       0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_minus3,uint,16,4) [] = { 0x2000, 0x2000, 0x2000, 0x2000 };
VECT_VAR_DECL(expected_minus3,uint,32,2) [] = { 0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_minus3,uint,64,1) [] = { 0x2000000000000000 };
VECT_VAR_DECL(expected_minus3,int,8,16) [] = { 0x10, 0x10, 0x10, 0x10,
					       0x10, 0x10, 0x10, 0x10,
					       0x10, 0x10, 0x10, 0x10,
					       0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_minus3,int,16,8) [] = { 0x1000, 0x1000, 0x1000, 0x1000,
					       0x1000, 0x1000, 0x1000, 0x1000 };
VECT_VAR_DECL(expected_minus3,int,32,4) [] = { 0x10000000, 0x10000000,
					       0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_minus3,int,64,2) [] = { 0x1000000000000000,
					       0x1000000000000000 };
VECT_VAR_DECL(expected_minus3,uint,8,16) [] = { 0x20, 0x20, 0x20, 0x20,
						0x20, 0x20, 0x20, 0x20,
						0x20, 0x20, 0x20, 0x20,
						0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_minus3,uint,16,8) [] = { 0x2000, 0x2000, 0x2000, 0x2000,
						0x2000, 0x2000, 0x2000, 0x2000 };
VECT_VAR_DECL(expected_minus3,uint,32,4) [] = { 0x20000000, 0x20000000,
						0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_minus3,uint,64,2) [] = { 0x2000000000000000,
						0x2000000000000000 };

/* Expected results with input=max and large shift amount.  */
VECT_VAR_DECL(expected_large_sh,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_large_sh,int,16,4) [] = { 0x7fff, 0x7fff,
						 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_large_sh,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_large_sh,int,64,1) [] = { 0x7fffffffffffffff };
VECT_VAR_DECL(expected_large_sh,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_large_sh,uint,16,4) [] = { 0xffff, 0xffff,
						  0xffff, 0xffff };
VECT_VAR_DECL(expected_large_sh,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_large_sh,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_large_sh,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						 0x7f, 0x7f, 0x7f, 0x7f,
						 0x7f, 0x7f, 0x7f, 0x7f,
						 0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_large_sh,int,16,8) [] = { 0x7fff, 0x7fff,
						 0x7fff, 0x7fff,
						 0x7fff, 0x7fff,
						 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_large_sh,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
						 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_large_sh,int,64,2) [] = { 0x7fffffffffffffff,
						 0x7fffffffffffffff };
VECT_VAR_DECL(expected_large_sh,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_large_sh,uint,16,8) [] = { 0xffff, 0xffff,
						  0xffff, 0xffff,
						  0xffff, 0xffff,
						  0xffff, 0xffff };
VECT_VAR_DECL(expected_large_sh,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						  0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_large_sh,uint,64,2) [] = { 0xffffffffffffffff,
						  0xffffffffffffffff };

/* Expected results with negative input and large shift amount.  */
VECT_VAR_DECL(expected_neg_large_sh,int,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						    0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_neg_large_sh,int,16,4) [] = { 0x8000, 0x8000,
						     0x8000, 0x8000 };
VECT_VAR_DECL(expected_neg_large_sh,int,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_neg_large_sh,int,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_neg_large_sh,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_neg_large_sh,uint,16,4) [] = { 0xffff, 0xffff,
						      0xffff, 0xffff };
VECT_VAR_DECL(expected_neg_large_sh,uint,32,2) [] = { 0xffffffff,
						      0xffffffff };
VECT_VAR_DECL(expected_neg_large_sh,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_neg_large_sh,int,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						     0x80, 0x80, 0x80, 0x80,
						     0x80, 0x80, 0x80, 0x80,
						     0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_neg_large_sh,int,16,8) [] = { 0x8000, 0x8000,
						     0x8000, 0x8000,
						     0x8000, 0x8000,
						     0x8000, 0x8000 };
VECT_VAR_DECL(expected_neg_large_sh,int,32,4) [] = { 0x80000000, 0x80000000,
						     0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_neg_large_sh,int,64,2) [] = { 0x8000000000000000,
						     0x8000000000000000 };
VECT_VAR_DECL(expected_neg_large_sh,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						      0xff, 0xff, 0xff, 0xff,
						      0xff, 0xff, 0xff, 0xff,
						      0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_neg_large_sh,uint,16,8) [] = { 0xffff, 0xffff,
						      0xffff, 0xffff,
						      0xffff, 0xffff,
						      0xffff, 0xffff };
VECT_VAR_DECL(expected_neg_large_sh,uint,32,4) [] = { 0xffffffff,
						      0xffffffff,
						      0xffffffff,
						      0xffffffff };
VECT_VAR_DECL(expected_neg_large_sh,uint,64,2) [] = { 0xffffffffffffffff,
						      0xffffffffffffffff };

/* Expected results with max/min input and large negative shift amount.  */
VECT_VAR_DECL(expected_large_neg_sh,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_neg_sh,uint,64,2) [] = { 0x0, 0x0 };

/* Expected results with input=0 and large negative shift amount.  */
VECT_VAR_DECL(expected_0_large_neg_sh,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						       0x0, 0x0, 0x0, 0x0,
						       0x0, 0x0, 0x0, 0x0,
						       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						       0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
							0x0, 0x0, 0x0, 0x0,
							0x0, 0x0, 0x0, 0x0,
							0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
							0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0_large_neg_sh,uint,64,2) [] = { 0x0, 0x0 };

#define INSN vqrshl
#define TEST_MSG "VQRSHL/VQRSHLQ"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: v3=vqrshl(v1,v2), then store the result.  */
#define TEST_VQRSHL2(INSN, T3, Q, T1, T2, W, N, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector_shift, T3, W, N));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N))

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQRSHL1(INSN, T3, Q, T1, T2, W, N, CMT) \
  TEST_VQRSHL2(INSN, T3, Q, T1, T2, W, N, CMT)

#define TEST_VQRSHL(T3, Q, T1, T2, W, N, CMT)	\
  TEST_VQRSHL1(INSN, T3, Q, T1, T2, W, N, CMT)

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
  /* Use values equal to or one-less-than the type width to check
     behavior on limits.  */
  VDUP(vector_shift, , int, s, 8, 8, 7);
  VDUP(vector_shift, , int, s, 16, 4, 15);
  VDUP(vector_shift, , int, s, 32, 2, 31);
  VDUP(vector_shift, , int, s, 64, 1, 63);
  VDUP(vector_shift, q, int, s, 8, 16, 8);
  VDUP(vector_shift, q, int, s, 16, 8, 16);
  VDUP(vector_shift, q, int, s, 32, 4, 32);
  VDUP(vector_shift, q, int, s, 64, 2, 64);

#define CMT " (with input = 0)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

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


  /* Use negative shift amounts.  */
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
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

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
  VDUP(vector_shift, , int, s, 64, 1, 3);
  VDUP(vector_shift, q, int, s, 8, 16, 10);
  VDUP(vector_shift, q, int, s, 16, 8, 12);
  VDUP(vector_shift, q, int, s, 32, 4, 31);
  VDUP(vector_shift, q, int, s, 64, 2, 63);

#undef CMT
#define CMT ""
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

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


  /* Use negative shift amounts.  */
  VDUP(vector_shift, , int, s, 8, 8, -2);
  VDUP(vector_shift, , int, s, 16, 4, -2);
  VDUP(vector_shift, , int, s, 32, 2, -3);
  VDUP(vector_shift, , int, s, 64, 1, -4);
  VDUP(vector_shift, q, int, s, 8, 16, -7);
  VDUP(vector_shift, q, int, s, 16, 8, -11);
  VDUP(vector_shift, q, int, s, 32, 4, -13);
  VDUP(vector_shift, q, int, s, 64, 2, -20);

#undef CMT
#define CMT " (negative shift amount)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

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


  /* Fill input vector with max value, to check saturation on
     limits.  */
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

  /* Use -1 shift amount to check cumulative saturation with
     round_const.  */
  VDUP(vector_shift, , int, s, 8, 8, -1);
  VDUP(vector_shift, , int, s, 16, 4, -1);
  VDUP(vector_shift, , int, s, 32, 2, -1);
  VDUP(vector_shift, , int, s, 64, 1, -1);
  VDUP(vector_shift, q, int, s, 8, 16, -1);
  VDUP(vector_shift, q, int, s, 16, 8, -1);
  VDUP(vector_shift, q, int, s, 32, 4, -1);
  VDUP(vector_shift, q, int, s, 64, 2, -1);

#undef CMT
#define CMT " (checking cumulative saturation: shift by -1)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_minus1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_minus1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_minus1, CMT);


  /* Use -3 shift amount to check cumulative saturation with
     round_const. */
  VDUP(vector_shift, , int, s, 8, 8, -3);
  VDUP(vector_shift, , int, s, 16, 4, -3);
  VDUP(vector_shift, , int, s, 32, 2, -3);
  VDUP(vector_shift, , int, s, 64, 1, -3);
  VDUP(vector_shift, q, int, s, 8, 16, -3);
  VDUP(vector_shift, q, int, s, 16, 8, -3);
  VDUP(vector_shift, q, int, s, 32, 4, -3);
  VDUP(vector_shift, q, int, s, 64, 2, -3);

#undef CMT
#define CMT " (checking cumulative saturation: shift by -3)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_minus3, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_minus3, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_minus3, CMT);


  /* Use large shift amount.  */
  VDUP(vector_shift, , int, s, 8, 8, 10);
  VDUP(vector_shift, , int, s, 16, 4, 20);
  VDUP(vector_shift, , int, s, 32, 2, 40);
  VDUP(vector_shift, , int, s, 64, 1, 70);
  VDUP(vector_shift, q, int, s, 8, 16, 10);
  VDUP(vector_shift, q, int, s, 16, 8, 20);
  VDUP(vector_shift, q, int, s, 32, 4, 40);
  VDUP(vector_shift, q, int, s, 64, 2, 70);

#undef CMT
#define CMT " (checking cumulative saturation: large shift amount)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_large_sh, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_large_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_large_sh, CMT);


  /* Fill input vector with negative values, to check saturation on
     limits.  */
  VDUP(vector, , int, s, 8, 8, 0x80);
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, , int, s, 64, 1, 0x8000000000000000LL);
  VDUP(vector, q, int, s, 8, 16, 0x80);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector, q, int, s, 64, 2, 0x8000000000000000LL);

  /* Use large shift amount.  */
  VDUP(vector_shift, , int, s, 8, 8, 10);
  VDUP(vector_shift, , int, s, 16, 4, 20);
  VDUP(vector_shift, , int, s, 32, 2, 40);
  VDUP(vector_shift, , int, s, 64, 1, 70);
  VDUP(vector_shift, q, int, s, 8, 16, 10);
  VDUP(vector_shift, q, int, s, 16, 8, 20);
  VDUP(vector_shift, q, int, s, 32, 4, 40);
  VDUP(vector_shift, q, int, s, 64, 2, 70);

#undef CMT
#define CMT " (checking cumulative saturation: large shift amount with negative input)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg_large_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_neg_large_sh, CMT);


  /* Fill input vector with negative and positive values, to check
   * saturation on limits */
  VDUP(vector, , int, s, 8, 8, 0x7F);
  VDUP(vector, , int, s, 16, 4, 0x7FFF);
  VDUP(vector, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, int, s, 8, 16, 0x80);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector, q, int, s, 64, 2, 0x8000000000000000LL);

  /* Use large negative shift amount  */
  VDUP(vector_shift, , int, s, 8, 8, -10);
  VDUP(vector_shift, , int, s, 16, 4, -20);
  VDUP(vector_shift, , int, s, 32, 2, -40);
  VDUP(vector_shift, , int, s, 64, 1, -70);
  VDUP(vector_shift, q, int, s, 8, 16, -10);
  VDUP(vector_shift, q, int, s, 16, 8, -20);
  VDUP(vector_shift, q, int, s, 32, 4, -40);
  VDUP(vector_shift, q, int, s, 64, 2, -70);

#undef CMT
#define CMT " (checking cumulative saturation: large negative shift amount)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_large_neg_sh, CMT);


  /* Fill input vector with 0, to check saturation in case of large
   * shift amount */
  VDUP(vector, , int, s, 8, 8, 0);
  VDUP(vector, , int, s, 16, 4, 0);
  VDUP(vector, , int, s, 32, 2, 0);
  VDUP(vector, , int, s, 64, 1, 0);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);

  /* Use large shift amount  */
  VDUP(vector_shift, , int, s, 8, 8, -10);
  VDUP(vector_shift, , int, s, 16, 4, -20);
  VDUP(vector_shift, , int, s, 32, 2, -40);
  VDUP(vector_shift, , int, s, 64, 1, -70);
  VDUP(vector_shift, q, int, s, 8, 16, -10);
  VDUP(vector_shift, q, int, s, 16, 8, -20);
  VDUP(vector_shift, q, int, s, 32, 4, -40);
  VDUP(vector_shift, q, int, s, 64, 2, -70);

#undef CMT
#define CMT " (checking cumulative saturation: large negative shift amount with 0 input)"
  TEST_VQRSHL(int, , int, s, 8, 8, CMT);
  TEST_VQRSHL(int, , int, s, 16, 4, CMT);
  TEST_VQRSHL(int, , int, s, 32, 2, CMT);
  TEST_VQRSHL(int, , int, s, 64, 1, CMT);
  TEST_VQRSHL(int, , uint, u, 8, 8, CMT);
  TEST_VQRSHL(int, , uint, u, 16, 4, CMT);
  TEST_VQRSHL(int, , uint, u, 32, 2, CMT);
  TEST_VQRSHL(int, , uint, u, 64, 1, CMT);
  TEST_VQRSHL(int, q, int, s, 8, 16, CMT);
  TEST_VQRSHL(int, q, int, s, 16, 8, CMT);
  TEST_VQRSHL(int, q, int, s, 32, 4, CMT);
  TEST_VQRSHL(int, q, int, s, 64, 2, CMT);
  TEST_VQRSHL(int, q, uint, u, 8, 16, CMT);
  TEST_VQRSHL(int, q, uint, u, 16, 8, CMT);
  TEST_VQRSHL(int, q, uint, u, 32, 4, CMT);
  TEST_VQRSHL(int, q, uint, u, 64, 2, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_large_neg_sh, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_large_neg_sh, CMT);
}

int main (void)
{
  exec_vqrshl ();
  return 0;
}
