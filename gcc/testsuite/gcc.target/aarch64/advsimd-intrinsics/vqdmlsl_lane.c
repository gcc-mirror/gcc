#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vqdmlsl_lane
#define TEST_MSG "VQDMLSL_LANE"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffff83c2, 0xffff83c3,
					0xffff83c4, 0xffff83c5 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffff83c2,
					0xffffffffffff83c3 };

/* Expected values of cumulative_saturation flag when multiplying with
   0.  */
int VECT_VAR(expected_cumulative_sat2,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat2,int,64,2) = 0;

/* Expected values when multiplying with 0.  */
VECT_VAR_DECL(expected2,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff1 };

/* Expected values of cumulative_saturation flag when multiplication
   saturates.  */
int VECT_VAR(expected_cumulative_sat3,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat3,int,64,2) = 1;

/* Expected values when multiplication saturates.  */
VECT_VAR_DECL(expected3,int,32,4) [] = { 0x80000000, 0x80000000,
					 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected3,int,64,2) [] = { 0x8000000000000000,
					 0x8000000000000000 };

#include "vqdmlXl_lane.inc"
