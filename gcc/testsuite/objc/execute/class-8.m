/* Contributed by Nicola Pero - Tue Mar  6 23:05:53 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>

#include "next_mapping.h"

/* Tests creating a root class and a subclass with an ivar and
   accessor methods and a subclass overriding the superclass'
   implementation - in a category */

@interface RootClass
{
  Class isa;
}
@end

@implementation RootClass
#ifdef __NEXT_RUNTIME__                                   
+ initialize { return self; }
#endif
@end

@interface SubClass : RootClass
{
  int state;
}
- (void) setState: (int)number;
- (int) state;
@end

@implementation SubClass
- (void) setState: (int)number
{
  state = number;
}
- (int) state
{
  return state;
}
@end

@interface SubSubClass : SubClass
@end

@implementation SubSubClass
@end

@implementation SubSubClass (Additions)
- (int) state
{
  return state + 1;
}
@end

#include "class-tests-1.h"
#define TYPE_OF_OBJECT_WITH_ACCESSOR_METHOD SubClass *
#include "class-tests-2.h"

int main (void)
{
  SubClass *object;
  SubSubClass *sub_object;

  test_class_with_superclass ("SubClass", "RootClass");
  test_that_class_has_instance_method ("SubClass", @selector (setState:));
  test_that_class_has_instance_method ("SubClass", @selector (state));

  test_class_with_superclass ("SubSubClass", "SubClass");
  test_that_class_has_instance_method ("SubSubClass", @selector (setState:));
  test_that_class_has_instance_method ("SubSubClass", @selector (state));
  
  object = class_create_instance (objc_lookup_class ("SubClass"));
  test_accessor_method (object, 0, -1, -1, 1, 1);

  sub_object = class_create_instance (objc_lookup_class ("SubSubClass"));
  test_accessor_method (sub_object, 1, -1, 0, 1, 2);

  return 0;
}
