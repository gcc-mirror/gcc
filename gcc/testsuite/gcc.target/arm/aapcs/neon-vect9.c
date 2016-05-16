/* Test AAPCS layout (VFP variant for Neon types) */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-add-options arm_neon_fp16 } */

#ifndef IN_FRAMEWORK
#define VFP
#define NEON
#define TESTFILE "neon-vect9.c"
#include "neon-constants.h"

#include "abitest.h"
#else

ARG (int32x4_t, i32x4_constvec2, Q0) /* D0, D1.  */
#if defined (__ARM_BIG_ENDIAN)
ARG (__fp16, 3.0f, S4 + 2) /* D2, Q1 occupied.  */
#else
ARG (__fp16, 3.0f, S4) /* D2, Q1 occupied.  */
#endif
LAST_ARG (int, 3, R0)
#endif
