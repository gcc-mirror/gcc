/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @synthesize for a @property which is not declared directly in
   the @interface, but in a @protocol that the @interface conforms
   to.  */

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
@end

@implementation Test
@synthesize v1;
@synthesize v2 = _v2;
@end

int main (void)
{
  Test *object = [[Test alloc] init];

  /* Check that the synthesized methods exist and work.  Do not invoke
     them via property syntax - that is another test.  Here we just
     want to test the synthesis of the methods.  */
  [object setV1: 400];

  if ([object v1] != 400)
    abort ();

  [object setV2: 31];

  if ([object v2] != 31)
    abort ();

  return 0;
}
