/* { dg-do compile } */
#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface Derived: TestsuiteObject
@end

extern TestsuiteObject* foo(void);
static Derived *test(void)
{
   Derived *m = foo();   /* { dg-warning "initialization from distinct Objective\\-C type" } */

   return m;
}
