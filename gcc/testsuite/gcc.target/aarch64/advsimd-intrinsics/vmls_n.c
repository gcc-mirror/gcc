#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmls
#define TEST_MSG "VMLS_N"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfa4b, 0xfa4c, 0xfa4d, 0xfa4e };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffff4a6, 0xfffff4a7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xef01, 0xef02, 0xef03, 0xef04 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffe95c, 0xffffe95d };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc49bdeb8, 0xc49bbeb8 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xe3b7, 0xe3b8, 0xe3b9, 0xe3ba,
					0xe3bb, 0xe3bc, 0xe3bd, 0xe3be };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffde12, 0xffffde13,
					0xffffde14, 0xffffde15 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xd86d, 0xd86e, 0xd86f, 0xd870,
					 0xd871, 0xd872, 0xd873, 0xd874 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffd2c8, 0xffffd2c9,
					 0xffffd2ca, 0xffffd2cb };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc56a087b, 0xc569f87b,
					   0xc569e87b, 0xc569d87b };

#include "vmlX_n.inc"
