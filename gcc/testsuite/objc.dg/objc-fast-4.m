/* The code should call objc_msgSend directly, not through a pointer.  */
/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-options "-O0 -fnext-runtime" } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "-m64" } { "" } } */
/* Radar 4015820 */

#include <objc/Object.h>

void foo(void) {
  Object *o;
  [o++ free];
}
/* { dg-final { scan-assembler-not "L_objc_msgSend\\\$non_lazy_ptr" } } */
