/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test that properties can be unavailable.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int a;
}
@property int a __attribute__((unavailable));
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

  object.a = 40;      /* { dg-error "is unavailable" } */
  if (object.a != 40) /* { dg-error "is unavailable" } */
    abort ();

  return 0;
}
