/* Test AAPCS layout (alignment) - passing vectors in GPRs.  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_neon_ok  } */
/* { dg-options "-O" } */
/* { dg-add-options arm_neon } */

#ifndef IN_FRAMEWORK
#define TESTFILE "align4.c"

#define PCSATTR __attribute__((pcs("aapcs")))

#include <arm_neon.h>

typedef __attribute__((aligned (4))) int32x2_t unalignedvec;

unalignedvec a = {11, 13};
unalignedvec b = {17, 19};

#include "abitest.h"
#else
  ARG (int, 2, R0)
  /* Attribute suggests R1, but we should use natural alignment:  */
  ARG (unalignedvec, a, R2)
  ARG (int, 6, STACK)
  /* Attribute would suggest STACK + 4 but should be ignored:  */
  LAST_ARG (unalignedvec, b, STACK + 8)
#endif
