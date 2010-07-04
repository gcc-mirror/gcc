/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

#include "../objc-obj-c++-shared/Object1.h"

int main (int argc, const char * argv[]) {
  Object * pool = [Object new];
  int a;

  if ( 1 ) {
    
    @try {
      a = 1;
    }
    @catch (Object *e) {
      a = 2;
    }
    @finally {
      a = 3;
    }
  }
    
  [pool free];
  return 0;
}
