/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */

/* Test that properties are found even if implemented in superclasses.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
@property int a;
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

@interface MySubClass : MyRootClass
@end

@implementation MySubClass
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];

  object.a = 40;
  if (object.a != 40)
    abort ();

  return 0;
}
