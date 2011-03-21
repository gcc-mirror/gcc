/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

/* Test that properties can be deprecated.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
@property int a __attribute__((deprecated));
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize a;
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.a = 40;      /* { dg-warning "is deprecated" } */
  if (object.a != 40) /* { dg-warning "is deprecated" } */
    abort ();

  return 0;
}
