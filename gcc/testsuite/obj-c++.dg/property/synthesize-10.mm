/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @synthesize with bitfield instance variables.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int countA : 2;
  int countB : 3;
  int countC : 4;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@property (nonatomic) int countA;
@property (nonatomic) int countB;
@property (nonatomic) int countC;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize countA;
@synthesize countB;
@synthesize countC;
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.countA = 1;
  object.countB = 3;
  object.countC = 4;

  if (object.countA != 1)
    abort ();

  if (object.countB != 3)
    abort ();

  if (object.countC != 4)
    abort ();
  
  return 0;
}
