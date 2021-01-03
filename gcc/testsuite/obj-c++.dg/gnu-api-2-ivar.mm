/* Test the Modern GNU Objective-C Runtime API.

  This is test 'ivar', covering all functions starting with 'ivar'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
// { dg-additional-options "-Wno-objc-root-class" }

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

  std::cout << "Testing ivar_getName () ...\n";
  {
    Ivar ivar = class_getInstanceVariable (objc_getClass ("MySubClass"),
					   "variable_ivar");
   if (strcmp (ivar_getName (ivar), "variable_ivar") != 0)
      abort ();

   ivar = class_getInstanceVariable (objc_getClass ("MySubClass"),
				     "variable");
   if (ivar != 0)
      abort ();
  }

  std::cout << "Testing ivar_getOffset () ...\n";
  {
    Ivar ivar = class_getInstanceVariable (objc_getClass ("MyRootClass"),
					   "isa");
    if (ivar_getOffset (ivar) != 0)
      abort ();
  }

  std::cout << "Testing ivar_getTypeEncoding () ...\n";
  {
    Ivar ivar = class_getInstanceVariable (objc_getClass ("MySubClass"),
					   "variable_ivar");
    if (strcmp (ivar_getTypeEncoding (ivar), "@") != 0)
      abort ();
  }

  return (0);
}
