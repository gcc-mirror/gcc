/* Test the Modern GNU Objective-C Runtime API.

  This is test 'protocol', covering all functions starting with 'protocol'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

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

@protocol MyThirdProtocol <MySecondProtocol>
- (id) setAnotherVariable: (id)value;
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

  printf ("Testing protocol_conformsToProtocol ()...\n");
  {
    if (!protocol_conformsToProtocol (@protocol (MyProtocol),
				      @protocol (MyProtocol)))
      abort ();

    if (!protocol_conformsToProtocol (@protocol (MyThirdProtocol),
				      @protocol (MySecondProtocol)))
      abort ();

    if (protocol_conformsToProtocol (@protocol (MyProtocol),
				     @protocol (MySecondProtocol)))
      abort ();
  }

  printf ("Testing protocol_copyMethodDescriptionList ()...\n");
  {
    unsigned int count;
    struct objc_method_description *list;

    list = protocol_copyMethodDescriptionList (@protocol (MyThirdProtocol),
					       YES, YES, &count);
    
    if (count != 1)
      abort ();

    if (strcmp (sel_getName (list[0].name), "setAnotherVariable:") != 0)
      abort ();
    
    if (list[1].name != NULL  &&  list[1].types != NULL)
      abort ();
  }

  /* TODO: Test new ABI (when available).  */
  printf ("Testing protocol_copyPropertyList ()...\n");
  {
    unsigned int count;
    objc_property_t *list;

    list = protocol_copyPropertyList (@protocol (MyProtocol), &count);

    if (count != 0  ||  list != NULL)
      abort ();
  }

  printf ("Testing protocol_copyProtocolList ()...\n");
  {
    unsigned int count;
    Protocol **list;

    list = protocol_copyProtocolList (@protocol (MyThirdProtocol), &count);
    
    if (count != 1)
      abort ();

    if (strcmp (protocol_getName (list[0]), "MySecondProtocol") != 0)
      abort ();
    
    if (list[1] != NULL)
      abort ();
  }

  printf ("Testing protocol_getMethodDescription ()...\n");
  {
    struct objc_method_description description;

    description = protocol_getMethodDescription (@protocol (MySecondProtocol),
						 @selector (setVariable:),
						 YES, YES);
    if (description.name == NULL  &&  description.types == NULL)
      abort ();

    if (strcmp (sel_getName (description.name), "setVariable:") != 0)
      abort ();
  }

  printf ("Testing protocol_getName ()...\n");
  {
    if (strcmp (protocol_getName (@protocol (MyProtocol)), "MyProtocol") != 0)
      abort ();
  }

  /* TODO: Test new ABI (when available).  */
  printf ("Testing protocol_getProperty ()...\n");
  {
    objc_property_t property;

    property = protocol_getProperty (objc_getProtocol ("MyProtocol"), "someProperty",
				     YES, YES);

    if (property != NULL)
      abort ();
  }

  printf ("Testing protocol_isEqual ()...\n");
  {
    if (!protocol_isEqual (@protocol (MyProtocol),
			   @protocol (MyProtocol)))
      abort ();

    if (!protocol_isEqual (@protocol (MyProtocol),
			   objc_getProtocol ("MyProtocol")))
      abort ();
  }

  return 0;
}
