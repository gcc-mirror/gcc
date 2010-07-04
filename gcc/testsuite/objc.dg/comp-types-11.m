/* { dg-do compile } */
#include "../objc-obj-c++-shared/Object1.h"

@interface Derived: Object
@end

extern Object* foo(void);
static Derived *test(void)
{
   Derived *m = foo();   /* { dg-warning "initialization from distinct Objective\\-C type" } */

   return m;
}

