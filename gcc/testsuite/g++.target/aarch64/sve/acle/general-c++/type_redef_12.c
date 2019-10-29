/* { dg-do compile } */

typedef struct svint8x2_t svint8x2_t;

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {conflicting declaration 'typedef struct svint8x2_t svint8x2_t'} } */

svint8_t f (svint8x2_t x) { return x.__val[0]; } /* { dg-error {'x' has incomplete type} } */
