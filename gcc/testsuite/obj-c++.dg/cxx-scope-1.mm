/* Handle C++ scoping ('::') operators in ObjC message receivers gracefully.  */
/* Author: Ziemowit Laski  <zlaski@apple.com> */

/* { dg-do run } */

#include <objc/Object.h>
#include <stdlib.h>

@class Derived;

Derived *inst[3];

struct CxxClass {
  static Derived *get_instance(int);
};

Derived *CxxClass::get_instance(int offs) {
  return inst[offs];
}

@interface Derived: Object {
  int value;
}
-(id)initWithValue:(int)val;
-(int)derived_meth;
@end

@implementation Derived
-(id)initWithValue:(int)val {
  [super init];
  value = val;
  return self;
}
- (int)derived_meth {
  return value;
}
@end

int main(void) {
  int r;
  inst[1] = [[::Derived alloc] initWithValue:7];
  inst[2] = [[Derived alloc] initWithValue:77];

  r = [CxxClass::get_instance(2) derived_meth];
  if (r != 77)
    abort();

  r = [CxxClass::get_instance(1) derived_meth];
  if (r != 7)
    abort();

  return 0;
}
