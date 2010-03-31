/* Ensure there are no bizarre difficulties with accessing the 'isa' field of
  objects.  This field is named differently between GNU and NeXT runtimes so
  accessed via the CLASSPTRFIELD() macro defined in next-mapping.h */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"

@interface Object (Test)
- (Class) test1: (id)object;
@end

@interface Derived: Object
- (Class) test2: (id)object;
@end

@implementation Object (Test)

Class test1(id object) {
  Class cls = CLASSPTRFIELD(object);
  return cls;
}
- (Class) test1: (id)object {
  Class cls = CLASSPTRFIELD(object);
  return cls;
}

@end

@implementation Derived

Class test2(id object) {
  Class cls = CLASSPTRFIELD(object);
  return cls;
}
- (Class) test2: (id)object {
  Class cls = CLASSPTRFIELD(object);
  return cls;
}

@end

Class test3(id object) {
  Class cls = CLASSPTRFIELD(object);
  return cls;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
