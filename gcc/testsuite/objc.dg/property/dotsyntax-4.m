/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test the 'dot syntax' without a declarated property.  This tests
   syntax errors in the case where the object is a Class.  */


#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

static int a;
static id b;

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
+ (int) count;
+ (void) setCount: (int)value;
+ (id) next;
+ (void) setNext: (id)value;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
+ (int) count
{
  return a;
}
+ (void) setCount: (int)value
{
  a = value;
}
+ (id) next
{
  return b;
}
+ (void) setNext: (id)value
{
  b = value;
}
@end

int main (void)
{
  MyRootClass *object = [[MyRootClass alloc] init];

  MyRootClass.invalid = 40;      /* { dg-error "could not find setter.getter" } */
  if (MyRootClass.invalid != 40) /* { dg-error "could not find setter.getter" } */
    abort ();

  MyRootClass.;           /* { dg-error "expected identifier" } */
  if (MyRootClass.)       /* { dg-error "expected identifier" } */
    abort ();

  MyRootClass.int;        /* { dg-error "expected identifier" } */
  if (MyRootClass.int)    /* { dg-error "expected identifier" } */
    abort ();

  return 0;
}
