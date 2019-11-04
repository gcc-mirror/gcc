/* { dg-do compile } */

int svbool_t; /* { dg-message "note: previous declaration 'int svbool_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'typedef [^'\n]* svbool_t' redeclared as different kind of entity} } */
