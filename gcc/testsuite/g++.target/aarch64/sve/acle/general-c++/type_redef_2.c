/* { dg-do compile } */

int svint8_t; /* { dg-message "note: previous declaration 'int svint8_t" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'typedef [^'\n]* svint8_t' redeclared as different kind of entity} } */
