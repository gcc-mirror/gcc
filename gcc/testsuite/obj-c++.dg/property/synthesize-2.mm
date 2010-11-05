/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

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

@interface Test : MyRootClass
{
  int v1;
}
@property int v1;
/* TODO: Test more types of properties with different semantics
   (retain, copy, atomic, nonatomic, and test various C and
   Objective-C types).  */
@end

@implementation Test
@synthesize v1;
@end

int main ()
{
  Test *object = [[Test alloc] init];

  /* Check that the synthesized methods exist and work.  Do not invoke
     them via property syntax - that is another test.  Here we just
     want to test the synthesis of the methods.  */
  [object setV1: 400];

  if ([object v1] != 400)
    abort ();

  return (0);
}

