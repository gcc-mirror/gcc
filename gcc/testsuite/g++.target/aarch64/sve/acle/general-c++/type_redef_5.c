/* { dg-do compile } */

typedef int svbool_t; /* { dg-message "note: previous declaration as 'typedef int svbool_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {conflicting declaration '[^'\n]* svbool_t'} } */
