/* { dg-do compile } */

typedef __SVBool_t svbool_t; /* { dg-message "note: previous declaration of 'svbool_t' was here" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {redefinition of typedef 'svbool_t'} } */

