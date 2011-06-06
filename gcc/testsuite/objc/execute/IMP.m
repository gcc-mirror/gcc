/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */

#include <stdlib.h>
#include "../../objc-obj-c++-shared/runtime.h"

/* Test getting and calling the IMP of a method */

@interface TestClass
{
  Class isa;
}
+ (int) next: (int)a;
@end

@implementation TestClass
+ (int) next: (int)a
{
  return a + 1;
}
@end

int main (void)
{
  Class class;
  SEL selector;
  int (* imp) (id, SEL, int);
  
  class = objc_getClass ("TestClass");
  selector = @selector (next:);
  imp = (int (*)(id, SEL, int))method_getImplementation
    (class_getClassMethod (class, selector));
  
  if (imp (class, selector, 5) != 6)
    {
      abort ();
    }

  return 0;
}
