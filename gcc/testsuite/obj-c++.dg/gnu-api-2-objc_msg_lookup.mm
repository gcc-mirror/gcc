/* Test the Modern GNU Objective-C Runtime API.

   This is test 'objc_msg_lookup', covering objc_msg_lookup(),
   objc_msg_lookup_super() and struct objc_super.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>

/* For objc_msg_lookup(), objc_msg_lookup_super() and struct
   objc_super.  */
#include <objc/message.h>

#include <stdlib.h>
#include <iostream>
#include <cstring>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
- (int) test;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
- (int) test { return 20; }
@end

@interface MySubClass : MyRootClass
- (int) test;
@end

@implementation MySubClass
- (int) test { return 11; }
@end

int main ()
{
  /* Functions are tested in alphabetical order.  */

  std::cout << "Testing objc_msg_lookup () ...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];
    int (* test_IMP) (id receiver, SEL selector);

    test_IMP = (int (*)(id, SEL))objc_msg_lookup (object, @selector (test));    
    
    if (test_IMP (object, @selector (test)) != 11)
      abort ();
  }

  std::cout << "Testing objc_msg_lookup_super () ...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];
    struct objc_super super = { 0, 0 };
    int (* test_IMP) (id receiver, SEL selector);

    /* Get the implementation of -test for the superclass of object -
       as if we were calling [super test] inside a method
       implementation of object.  */
    super.self = object;
    super.super_class = class_getSuperclass (object_getClass (object));
    test_IMP = (int (*)(id, SEL))objc_msg_lookup_super (&super, @selector (test));

    /* Invoke it.  The method in MyRootClass, not the one in
       MySubClass, should be invoked.  */
    if (test_IMP (object, @selector (test)) != 20)
      abort ();
  }

  return (0);
}
