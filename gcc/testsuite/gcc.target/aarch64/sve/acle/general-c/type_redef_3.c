/* { dg-do compile } */

int svuint16_t; /* { dg-message "note: previous declaration of 'svuint16_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svuint16_t' redeclared} } */
