/* The code should call objc_msgSend directly, not through a pointer.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-O0" } */
/* Radar 4015820 */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

void foo(void) {
  TestsuiteObject *o;
  [o++ free];
}
/* { dg-final { scan-assembler-not "L_objc_msgSend\\\$non_lazy_ptr" } } */
