/* Test the Modern GNU Objective-C Runtime API.

  This is test 'sel', covering all functions starting with 'sel'.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

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


int main ()
{
  /* Functions are tested in alphabetical order.  */

  std::cout << "Testing sel_getName () ...\n";
  {
    if (std::strcmp (sel_getName (@selector (variable)), "variable") != 0)
      abort ();

    if (std::strcmp (sel_getName (NULL), "<null selector>") != 0)
      abort ();
  }

  std::cout << "Testing sel_getType () ...\n";
  {
    /* Get a selector from a real class, so it has interesting
       types.  */
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (variable));
    
    if (std::strcmp (sel_getType (method_getName (method)), method_getTypeEncoding (method)) != 0)
      abort ();
  }

  std::cout << "Testing sel_getUid () ...\n";
  {
    if (std::strcmp (sel_getName (sel_getUid ("myMethod")), "myMethod") != 0)
      abort ();
  }

  std::cout << "Testing sel_isEqual () ...\n";
  {
    if (! sel_isEqual (@selector (setVariable:), @selector (setVariable:)))
      abort ();
  }
  
  std::cout << "Testing sel_registerName () ...\n";
  {
    if (std::strcmp (sel_getName (sel_registerName ("myMethod")), "myMethod") != 0)
      abort ();
  }

  std::cout << "Testing set_registerTypedName () ...\n";
  {
    const char *types = method_getTypeEncoding (class_getInstanceMethod 
						(objc_getClass ("MySubClass"),
						 @selector (variable)));
    SEL selector = sel_registerTypedName ("aMethod", types);
	    
    if (std::strcmp (sel_getName (selector), "aMethod") != 0)
      abort ();

    if (std::strcmp (sel_getType (selector), types) != 0)
      abort ();
  }

  return (0);
}
