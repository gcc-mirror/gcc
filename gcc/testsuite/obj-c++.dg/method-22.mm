/* Ensure that overload resolution does not produce warnings as
   side-effects.  */
/* { dg-do run } */
/* { dg-additional-sources "../objc-obj-c++-shared/Object1.mm" } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

#define CHECK_IF(E) if(!(E)) abort ()

@interface MyCursor: Object
+ (MyCursor *)crosshairCursor;
@end

@class MyImage;

class A {
public:
    A();
    
    int foo(MyImage *);
    int foo(MyCursor *);
};

A::A() {}
int A::foo(MyCursor * c) { return 17; }
int A::foo(MyImage * i) { return 29; }

@implementation MyCursor
+ (MyCursor *)crosshairCursor {
  return self;
}
@end

int main(void) {
  A a;
  
  int r = a.foo([MyCursor crosshairCursor]);

  CHECK_IF (r == 17);    
  return 0;
}

