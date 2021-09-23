/* { dg-do compile } */

typedef int svbool_t; /* { dg-message "note: previous declaration of 'svbool_t'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {conflicting types for 'svbool_t'} } */
