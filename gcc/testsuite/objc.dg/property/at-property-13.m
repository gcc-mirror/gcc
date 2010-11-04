/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test retain and copy synthesized methods.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int copy_count;
  id a;
  id b;
}
@property (copy) id a;
@property (retain) id b;
+ (id) initialize;
+ (id) alloc;
- (id) init;
- (id) copyWithZone: (void *)zone;
- (int) copyCount;
- (id) autorelease;
- (oneway void) release;
- (id) retain;
@end

/* This class implements copyWithZone, which doesn't do anything other
   than increasing a counter of how many copies were made.  */
@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
- (id) copyWithZone: (void *)zone { copy_count++; return self; }
- (int) copyCount { return copy_count; }
- (id) autorelease { return self; }
- (oneway void) release { return; }
- (id) retain { return self; }
@synthesize a;
@synthesize b;
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];
  MyRootClass *argument = [[MyRootClass alloc] init];

  /* This should copy argument.  */
  object.a = argument;
  if (object.a != argument)
    abort ();

  /* Test that it was copied.  */
  if ([object.a copyCount] != 1)
    abort ();

  /* We just test that the retain accessors seem to work and that they
     don't copy.  We don't test that retain was actually called,
     because if garbage collection is enabled, it may never be
     called!  */
  object.b = argument;
  if (object.b != argument)
    abort ();

  /* Test that it was not copied.  */
  if ([object.b copyCount] != 1)
    abort ();

  return 0;
}
