/* { dg-do compile } */

/* Although not supported, there's nothing to stop the user overloading
   the sv* functions.  */
extern __SVInt8_t svadd_u8_x (__SVBool_t, __SVInt8_t, __SVInt8_t);

#pragma GCC aarch64 "arm_sve.h"
