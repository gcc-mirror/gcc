/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @synthesize for a @property where the setter/getter are also
   declared by the user.  This is fine.  */

#include <objc/objc.h>
#include <objc/runtime.h>
#include <stdlib.h>

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@end

@protocol MyProtocol
@property int v1;
@end

@protocol MyProtocol2
@property int v2;
@end

@interface Test : MyRootClass <MyProtocol, MyProtocol2>
{
  int v1;
  int _v2;
}
- (int)v1;
- (void)setV1: (int)aNumber;
- (int)v2;
@end

@implementation Test
@synthesize v1;
@synthesize v2 = _v2;
@end

int main (void)
{
  Test *object = [[Test alloc] init];

  /* We use dot-syntax here as this is just a general test that
     user-declared setters/getters don't cause confusion.  */
  object.v1 = 400;

  if (object.v1 != 400)
    abort ();

  object.v2 = 31;

  if (object.v2 != 31)
    abort ();

  return 0;
}
