/* { dg-do compile } */
/* { dg-options "-fobjc-exceptions" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

int main (int argc, const char * argv[]) {
  TestsuiteObject * pool = [TestsuiteObject new];
  int a;

  if ( 1 ) {
    
    @try {
      a = 1;
    }
    @catch (TestsuiteObject *e) {
      a = 2;
    }
    @finally {
      a = 3;
    }
  }
    
  [pool free];
  return 0;
}
