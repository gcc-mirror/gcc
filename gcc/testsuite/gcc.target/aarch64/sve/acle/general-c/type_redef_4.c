/* { dg-do compile } */

int svfloat32_t; /* { dg-message "note: previous declaration of 'svfloat32_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svfloat32_t' redeclared} } */
