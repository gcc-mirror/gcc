/* Test AAPCS layout (VFP variant for Neon types) */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm_hard_vfp_ok } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm32 } */
/* { dg-add-options arm_neon } */


#ifndef IN_FRAMEWORK
#define VFP
#define NEON
#define TESTFILE "neon-vect8.c"
#include "neon-constants.h"


#include "abitest.h"
#else

ARG(float, 24.3f, S0) /* S0 , D0, Q0 */
ARG(int32x2_t, i32x2_constvec1, D1) /* D1  */
ARG(double, 25.6, D2)
ARG(float, 12.67f, S1)
ARG(int32x4x3_t, i32x4x3_constvec2, STACK)
ARG(double, 2.47, STACK+sizeof(int32x4x3_t))
LAST_ARG(int, 3, R0)
#endif
