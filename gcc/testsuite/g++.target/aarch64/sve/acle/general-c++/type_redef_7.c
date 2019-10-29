/* { dg-do compile } */

int svint8x2_t;

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {'typedef struct svint8x2_t svint8x2_t' redeclared as different kind of entity} } */

void f (struct svint8x2_t) {} /* { dg-error {incomplete type} } */
void g () { int &x = svint8x2_t; }
