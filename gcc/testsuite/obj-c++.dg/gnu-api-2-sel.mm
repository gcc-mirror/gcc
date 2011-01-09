/* Test the Modern GNU Objective-C Runtime API.

  This is test 'sel', covering all functions starting with 'sel'.  */

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
- (void) method;
@end

@implementation MySubClass
- (void) setVariable: (id)value { variable_ivar = value; }
- (id) variable { return variable_ivar; }
- (void) method { return; }
@end


int main ()
{
  /* Functions are tested in alphabetical order.  */

#ifdef __GNU_LIBOBJC__
  std::cout << "Testing sel_copyTypedSelectorList ()...\n";
  {
    unsigned int count;
    SEL * list = sel_copyTypedSelectorList ("method", &count);

    /* There should only be two, since 'method' is referenced twice,
       once with types and once without (in this very test).  */
    if (count != 2)
      abort ();

    /* Check that both selectors are not-NULL, and have the correct
       name.  We use @selector() here, which wouldn't really be
       needed, just to register a second, untyped selector with name
       'method'.  */
    if (std::strcmp (sel_getName (list[0]), sel_getName (@selector (method))) != 0)
      abort ();

    if (std::strcmp (sel_getName (list[1]), sel_getName (@selector (method))) != 0)
      abort ();
    
    if (list[2] != NULL)
      abort ();
  }
#endif

  std::cout << "Testing sel_getName () ...\n";
  {
    if (std::strcmp (sel_getName (@selector (variable)), "variable") != 0)
      abort ();

    if (std::strcmp (sel_getName (NULL), "<null selector>") != 0)
      abort ();
  }

#ifdef __GNU_LIBOBJC__
  std::cout << "Testing sel_getTypeEncoding () ...\n";
  {
    /* Get a selector from a real class, so it has interesting
       types.  */
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (variable));
    
    if (std::strcmp (sel_getTypeEncoding (method_getName (method)),
		method_getTypeEncoding (method)) != 0)
      abort ();

    if (sel_getTypeEncoding (NULL) != NULL)
      abort ();
  }
#endif

#ifdef __GNU_LIBOBJC__
  std::cout << "Testing sel_getTypedSelector () ...\n";
  {
    /* First try with a selector where we know that a typed one has
       been registered.  */
    SEL selector = sel_getTypedSelector ("variable");

    if (selector == NULL)
      abort ();

    if (sel_getTypeEncoding (selector) == NULL)
      abort ();

    /* Now try a selector which was never registered.  */
    selector = sel_getTypedSelector ("not_registered");

    if (selector != NULL)
      abort ();

    /* Now try registering a selector with no types.  The following
       line is just a way to have an unused '@selector()' expression
       without the compiler complaining.  */
    if (@selector (registered_with_no_types) == NULL)
      abort ();

    /* Try getting it.  Nothing should be returned because it is
       untyped.  */
    selector = sel_getTypedSelector ("registered_with_no_types");

    if (selector != NULL)
      abort ();
  }
#endif

  std::cout << "Testing sel_getUid () ...\n";
  {
    if (std::strcmp (sel_getName (sel_getUid ("myMethod")), "myMethod") != 0)
      abort ();

    if (sel_getUid (NULL) != NULL)
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

    if (sel_registerName (NULL) != NULL)
      abort ();
  }

#ifdef __GNU_LIBOBJC__
  std::cout << "Testing set_registerTypedName () ...\n";
  {
    const char *types = method_getTypeEncoding (class_getInstanceMethod 
						(objc_getClass ("MySubClass"),
						 @selector (variable)));
    SEL selector = sel_registerTypedName ("aMethod", types);
    
    if (std::strcmp (sel_getName (selector), "aMethod") != 0)
      abort ();

    if (std::strcmp (sel_getTypeEncoding (selector), types) != 0)
      abort ();

    if (sel_registerTypedName (NULL, NULL) != NULL)
      abort ();

    if (sel_registerTypedName (NULL, types) != NULL)
      abort ();
  }
#endif

  return (0);
}
