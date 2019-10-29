/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h"

int svint8x2_t; /* { dg-error {'int svint8x2_t' redeclared as different kind of entity} } */

void f (struct svint8x2_t) {} /* { dg-error {using typedef-name 'svint8x2_t' after 'struct'} } */
void g () { int &x = svint8x2_t; } /* { dg-error {expected primary-expression before ';' token} } */
