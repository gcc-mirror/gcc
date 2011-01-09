/* Test the Modern GNU Objective-C Runtime API.

  This is test 'property', covering all functions starting with 'property'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <iostream>
#include <cstring>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
+ initialize;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
+ initialize { return self; }
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


int main ()
{
  /* Functions are tested in alphabetical order.  */

  /* TODO: Test new ABI (when available).  */
  std::cout << "Testing property_getAttributes () ...\n";
  {
    /* The Apple/NeXT runtime seems to crash on the following.  */
#ifdef __GNU_LIBOBJC__
    if (property_getAttributes (NULL) != NULL)
      abort ();
#endif
  }

  /* TODO: Test new ABI (when available).  */
  std::cout << "Testing property_getName () ...\n";
  {
    /* The Apple/NeXT runtime seems to crash on the following.  */
#ifdef __GNU_LIBOBJC__

    if (property_getName (NULL) != NULL)
      abort ();
#endif
  }

  return (0);
}
