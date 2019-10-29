/* { dg-do compile } */

int svint8_t; /* { dg-message "note: previous declaration of 'svint8_t' was here" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svint8_t' redeclared} } */
