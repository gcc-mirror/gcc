/* Test that we get warnings for unrecognized attributes.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
/* TODO: Emit warnings in the @interface as well.  Currently we only emit
   them in @implementation.  */
+ (id) method1: (id) __attribute__ ((xxxxx)) argument1;
+ (id) method2: (id) __attribute__ ((noinline)) argument1;
@end

@implementation MyRootClass
+ (id) method1: (id) __attribute__ ((xxxxx)) argument1
{  /* { dg-warning ".xxxxx. attribute directive ignored" } */
  return argument1;
}
+ (id) method2: (id) __attribute__ ((noinline)) argument1
{ /* { dg-warning ".noinline. attribute ignored" } */
  return argument1;
}
@end
