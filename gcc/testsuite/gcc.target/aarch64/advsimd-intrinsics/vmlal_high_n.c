/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vmlal_high_n
#define TEST_MSG "VMLAL_HIGH_N"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x595, 0x596, 0x597, 0x598 };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0xb3a, 0xb3b };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x10df, 0x10e0, 0x10e1, 0x10e2 };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0x10df, 0x10e0 };

#include "vmlXl_high_n.inc"
