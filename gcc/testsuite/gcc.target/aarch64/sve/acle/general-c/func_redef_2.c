/* { dg-do compile } */

int svadd_n_u8_x = 1; /* { dg-message "note: previous definition of 'svadd_n_u8_x' was here" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {'svadd_n_u8_x' redeclared} } */
