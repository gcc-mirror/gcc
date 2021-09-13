/* { dg-do compile } */

extern __SVInt8_t svadd_u8_x (__SVBool_t, __SVInt8_t, __SVInt8_t); /* { dg-message "note: previous declaration of 'svadd_u8_x'" } */

#pragma GCC aarch64 "arm_sve.h"  /* { dg-error {conflicting types for 'svadd_u8_x'} } */
