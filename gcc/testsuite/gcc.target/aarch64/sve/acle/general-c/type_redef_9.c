/* { dg-do compile } */

typedef int svint8x2_t; /* { dg-message "note: previous declaration of 'svint8x2_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {conflicting types for 'svint8x2_t'} } */
