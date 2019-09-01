/* Ensure there are no bizarre difficulties with accessing the 'isa' field of objects.  */
/* { dg-do compile } */
/* The use of isa is deprecated, but we still want to test that it works. */
/* { dg-additional-options "-Wno-deprecated-declarations" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include "../objc-obj-c++-shared/runtime.h"

@interface TestsuiteObject (Test)
- (Class) test1: (id)object;
@end

@interface Derived: TestsuiteObject
- (Class) test2: (id)object;
@end

@implementation TestsuiteObject (Test)

Class test1(id object) {
#ifdef __NEXT_RUNTIME__
    Class cls = object->isa;
#else
    Class cls = object->class_pointer;
#endif
    return cls;
}
- (Class) test1: (id)object {
#ifdef __NEXT_RUNTIME__
    Class cls = object->isa;
#else
    Class cls = object->class_pointer;
#endif
    return cls;
}

@end

@implementation Derived

Class test2(id object) {
#ifdef __NEXT_RUNTIME__
    Class cls = object->isa;
#else
    Class cls = object->class_pointer;
#endif
    return cls;
}
- (Class) test2: (id)object {
#ifdef __NEXT_RUNTIME__
    Class cls = object->isa;
#else
    Class cls = object->class_pointer;
#endif
    return cls;
}

@end

Class test3(id object) {
#ifdef __NEXT_RUNTIME__
    Class cls = object->isa;
#else
    Class cls = object->class_pointer;
#endif
    return cls;
}
