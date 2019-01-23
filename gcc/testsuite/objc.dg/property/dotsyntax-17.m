/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test errors with the dot-syntax with pre/post increment and decrement.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int count;
  int a;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@property (assign, readonly) int count;
- (void) setWriteOnlyCount: (int)value;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize count;
- (void) setWriteOnlyCount: (int)value
{
  a = value;
}
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  object.count = 10; /* { dg-error "readonly property cannot be set" } */
  if (object.count != 10) /* Ok */
    abort ();

  /* Test errors when trying to change a readonly property using
     pre/post increment/decrement operators.  */
  object.count++; /* { dg-error "readonly property cannot be set" } */

  ++object.count; /* { dg-error "readonly property cannot be set" } */

  object.count--; /* { dg-error "readonly property cannot be set" } */

  --object.count; /* { dg-error "readonly property cannot be set" } */

  /* Test errors when trying to change something using Objective-C 2.0
     dot-syntax but there is a setter but no getter.  */
  object.writeOnlyCount = 10; /* Ok */

  object.writeOnlyCount++; /* { dg-error "no .writeOnlyCount. getter found" } */

  ++object.writeOnlyCount; /* { dg-error "no .writeOnlyCount. getter found" } */

  object.writeOnlyCount--; /* { dg-error "no .writeOnlyCount. getter found" } */

  --object.writeOnlyCount; /* { dg-error "no .writeOnlyCount. getter found" } */

  return 0;
}


