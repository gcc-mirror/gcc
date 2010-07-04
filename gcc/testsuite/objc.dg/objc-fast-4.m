/* The code should call objc_msgSend directly, not through a pointer.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-O0" } */
/* Radar 4015820 */

#include "../objc-obj-c++-shared/Object1.h"

void foo(void) {
  Object *o;
  [o++ free];
}
/* { dg-final { scan-assembler-not "L_objc_msgSend\\\$non_lazy_ptr" } } */
