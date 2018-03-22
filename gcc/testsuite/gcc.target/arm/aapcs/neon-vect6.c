/* Test AAPCS layout (VFP variant for Neon types) */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-require-effective-target arm32 } */
/* { dg-add-options arm_neon } */


#ifndef IN_FRAMEWORK
#define VFP
#define NEON
#define TESTFILE "neon-vect6.c"
#include "neon-constants.h"


#include "abitest.h"
#else

ARG(int32x4_t, i32x4_constvec2, Q0) /* D0, D1 */
ARG(int32x4x3_t, i32x4x3_constvec1, Q1) /* Q1, Q2, Q3  */
ARG(int32x4x3_t, i32x4x3_constvec2, STACK)
LAST_ARG(int, 3, R0)
#endif
