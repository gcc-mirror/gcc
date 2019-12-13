/* { dg-do compile } */
/* { dg-options "-std=gnu90" } */

typedef __SVBool_t svbool_t;

/* Without -pedantic-errors this should compile.  */
#pragma GCC aarch64 "arm_sve.h"
