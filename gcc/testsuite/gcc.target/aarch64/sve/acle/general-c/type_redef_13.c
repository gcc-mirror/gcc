/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-message "note: previous declaration of 'svint8x2_t'" } */

int svint8x2_t;  /* { dg-error {'svint8x2_t' redeclared} } */
