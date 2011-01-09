/* Test the Modern GNU Objective-C Runtime API.

  This is test 'method', covering all functions starting with 'method'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

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
- (id) constant;
@end

@implementation MySubClass
- (void) setVariable: (id)value { variable_ivar = value; }
- (id) variable { return variable_ivar; }
- (id) constant { return nil; }
@end


int main ()
{
  /* Functions are tested in alphabetical order.  */

  std::cout <<"Testing method_copyArgumentType () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    char *type = method_copyArgumentType (method, 2);

    if (type == NULL  ||  type[0] != '@')
      abort ();
  }

  std::cout << "Testing method_copyReturnType () ...\n";
  {
    Method method = class_getClassMethod (objc_getClass ("MyRootClass"),
					  @selector (alloc));
    char *type = method_copyReturnType (method);

    /* Check that it returns an object.  */
    if (type == NULL  ||  type[0] != '@')
      abort ();
  }

  std::cout << "Testing method_exchangeImplementations () ...\n";
  {
    Method method_a = class_getInstanceMethod (objc_getClass ("MySubClass"),
					       @selector (variable));
    Method method_b = class_getInstanceMethod (objc_getClass ("MySubClass"),
					       @selector (constant));
    MySubClass *object = [[MySubClass alloc] init];

    /* Check that things work as expected before the swap.  */
    [object setVariable: object];

    if ([object variable] != object  ||  [object constant] != nil)
      abort ();

    /* Swap the methods.  */
    method_exchangeImplementations (method_a, method_b);

    /* Check that behaviour has changed.  */
    if ([object variable] != nil  ||  [object constant] != object)
      abort ();

    /* Swap the methods again.  */
    method_exchangeImplementations (method_a, method_b);
    
    /* Check that behaviour is back to normal.  */
    if ([object variable] != object  ||  [object constant] != nil)
      abort ();
  }

  std::cout << "Testing method_getArgumentType () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MyRootClass"),
					     @selector (init));
    char type[16];
    
    method_getArgumentType (method, 1, type, 16);

    /* Check the second argument (_cmd), which should be a SEL.  */
    if (type[0] != ':')
      abort ();
  }

  std::cout << "Testing method_getDescription () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (variable));
    struct objc_method_description *description = method_getDescription (method);

    if (std::strcmp (sel_getName (description->name), "variable") != 0)
      abort ();

    if (method_getDescription (NULL) != NULL)
      abort ();
  }

  std::cout << "Testing method_getImplementation () ...\n";
  {
    typedef void (*set_variable_function) (id receiver, SEL _cmd, id variable);
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    set_variable_function imp;
    MySubClass *object = [[MySubClass alloc] init];

    imp = (set_variable_function)(method_getImplementation (method));
    
    (*imp)(object, @selector (setVariable:), object);

    if ([object variable] != object)
      abort ();
  }

  std::cout << "Testing method_getName () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    if (std::strcmp (sel_getName (method_getName (method)), "setVariable:") != 0)
      abort ();
  }

  std::cout << "Testing method_getNumberOfArguments () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    if (method_getNumberOfArguments (method) != 3)
      abort ();

    method = class_getInstanceMethod (objc_getClass ("MySubClass"),
				      @selector (variable));
    if (method_getNumberOfArguments (method) != 2)
      abort ();
  }

  std::cout << "Testing method_getTypeEncoding () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    const char *types = method_getTypeEncoding (method);

    /* Check that method type string starts with 'v' (void)  */
    if (types == NULL || types[0] != 'v')
      abort ();    
  }

  std::cout << "Testing method_getReturnType () ...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"),
					     @selector (setVariable:));
    char type[16];
    
    method_getReturnType (method, type, 16);

    if (type[0] != 'v')
      abort ();

    method_getReturnType (NULL, type, 16);

    if (type[0] != 0)
      abort ();
  }

  std::cout << "Testing method_setImplementation () ...\n";
  {
    Method method_a = class_getInstanceMethod (objc_getClass ("MySubClass"),
					       @selector (variable));
    Method method_b = class_getInstanceMethod (objc_getClass ("MySubClass"),
					       @selector (constant));
    IMP original_imp_a = method_getImplementation (method_a);
    IMP original_imp_b = method_getImplementation (method_b);
    MySubClass *object = [[MySubClass alloc] init];

    /* Check that things work as expected before the swap.  */
    [object setVariable: object];
    
    if ([object variable] != object  ||  [object constant] != nil)
      abort ();
    
    /* Have 'variable' use the same implementation as 'constant'.  */
    if (method_setImplementation (method_a, original_imp_b) != original_imp_a)
      abort ();

    /* Check that behaviour has changed.  */
    if ([object variable] != nil  ||  [object constant] != nil)
      abort ();

    /* Put the original method back.  */
    if (method_setImplementation (method_a, original_imp_a) != original_imp_b)
      abort ();
    
    /* Check that behaviour is back to normal.  */
    if ([object variable] != object  ||  [object constant] != nil)
      abort ();
  }

  return (0);
}
