/* { dg-do compile } */

int svadd_n_u8_x; /* { dg-message "note: previous declaration of 'svadd_n_u8_x' was here" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svadd_n_u8_x' redeclared} } */
