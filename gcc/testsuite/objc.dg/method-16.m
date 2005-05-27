/* Do not warn about "slightly" mismatched method signatures if 
   -Wstrict-selector-match is off.  */

/* { dg-do compile } */
/* { dg-options "-Wno-strict-selector-match" } */

#include <objc/objc.h>

@interface Base
- (id) meth1: (Base *)arg1;
- (id) window;
@end

@interface Derived: Base
- (id) meth1: (Derived *)arg1;
- (Base *)window;
@end

void foo(void) {
  id r;

  [r meth1:r];
  [r window];
}
