/* { dg-do compile } */

int svbool_t; /* { dg-message "note: previous declaration of 'svbool_t' was here" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svbool_t' redeclared} } */
