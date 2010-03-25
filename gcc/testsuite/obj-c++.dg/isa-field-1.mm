/* Ensure there are no bizarre difficulties with accessing the 'isa' field of objects.  */
/* { dg-do compile } */
/* { dg-xfail-if "PR23613" { "*-*-*" } { "*" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"

@interface Object (Test)
- (Class) test1: (id)object;
@end

@interface Derived: Object
- (Class) test2: (id)object;
@end

@implementation Object (Test)

Class test1(id object) {
  Class cls = object->isa;
  return cls;
}
- (Class) test1: (id)object {
  Class cls = object->isa;
  return cls;
}

@end

@implementation Derived

Class test2(id object) {
  Class cls = object->isa;
  return cls;
}
- (Class) test2: (id)object {
  Class cls = object->isa;
  return cls;
}

@end

Class test3(id object) {
  Class cls = object->isa;
  return cls;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
