/* { dg-do compile } */

int svfloat32_t; /* { dg-message "note: previous declaration 'int svfloat32_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'typedef [^'\n]* svfloat32_t' redeclared as different kind of entity} } */
