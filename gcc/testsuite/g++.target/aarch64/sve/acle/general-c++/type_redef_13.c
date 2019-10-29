/* { dg-do compile } */

struct svint8x2_t {};

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {conflicting declaration 'typedef struct svint8x2_t svint8x2_t'} } */
