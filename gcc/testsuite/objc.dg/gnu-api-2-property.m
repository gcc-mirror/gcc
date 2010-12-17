/* Test the Modern GNU Objective-C Runtime API.

  This is test 'property', covering all functions starting with 'property'.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
@end

@protocol MyProtocol
- (id) variable;
@end

@protocol MySecondProtocol
- (id) setVariable: (id)value;
@end

@interface MySubClass : MyRootClass <MyProtocol>
{ id variable_ivar; }
- (void) setVariable: (id)value;
- (id) variable;
@end

@implementation MySubClass
- (void) setVariable: (id)value { variable_ivar = value; }
- (id) variable { return variable_ivar; }
@end


int main(int argc, void **args)
{
  /* Functions are tested in alphabetical order.  */

  /* TODO: Test new ABI (when available).  */
  printf ("Testing property_getAttributes () ...\n");
  {
    if (property_getAttributes (NULL) != NULL)
      abort ();
  }

  /* TODO: Test new ABI (when available).  */
  printf ("Testing property_getName () ...\n");
  {
    if (property_getName (NULL) != NULL)
      abort ();
  }

  return 0;
}
