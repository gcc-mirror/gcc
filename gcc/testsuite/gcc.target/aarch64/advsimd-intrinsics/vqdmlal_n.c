#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vqdmlal_n
#define TEST_MSG "VQDMLAL_N"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0x1684, 0x1685, 0x1686, 0x1687 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x21ce, 0x21cf };

/* Expected values of cumulative_saturation flag when saturation
   occurs.  */
int VECT_VAR(expected_cumulative_sat2,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat2,int,64,2) = 1;

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected2,int,32,4) [] = { 0x7fffffef, 0x7ffffff0,
					 0x7ffffff1, 0x7ffffff2 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0x7fffffffffffffef,
					 0x7ffffffffffffff0 };

#include "vqdmlXl_n.inc"
