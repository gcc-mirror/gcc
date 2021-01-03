/* Contributed by Nicola Pero - Fri Dec 14 08:36:00 GMT 2001 */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/runtime.h"

extern void abort (void);

/* Test loading unclaimed categories - categories of a class defined
   separately from the class itself.  */


/* unclaimed-category-1.m contains only the class definition but not
   the categories.  unclaimed-category-1a.m contains only the
   categories of the class, but not the class itself.  We want to
   check that the runtime can load the class from one module (file)
   and the categories from another module (file).  */

#include "unclaimed-category-1.h"

@implementation TestClass
- (int)D
{
  return 4;
}
+ initialize { return self; }
@end


int main (void)
{
  TestClass *test;
  Class testClass;

  testClass = objc_getClass ("TestClass");
  if (testClass == Nil)
    {
      abort ();
    }
  
  test = (TestClass *)(class_createInstance (testClass, 0));
  if (test == nil)
    {
      abort ();
    }
  
  if ([test A] != 1)
    {
      abort ();
    }
  
  if ([test B] != 2)
    {
      abort ();
    }

  if ([test C] != 3)
    {
      abort ();
    }
  

  if ([test D] != 4)
    {
      abort ();
    }

  return 0;
}
