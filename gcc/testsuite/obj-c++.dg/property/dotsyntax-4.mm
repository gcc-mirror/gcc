/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test the 'dot syntax' without a declarated property.  This tests
   syntax errors in the case where the object is a Class.  */


#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

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

int main (void)
{
  MyRootClass.invalid = 40;      /* { dg-error "could not find setter.getter" } */
  if (MyRootClass.invalid != 40) /* { dg-error "could not find setter.getter" } */
    abort ();

  MyRootClass.;           /* { dg-error "expected identifier" } */
  if (MyRootClass.)       /* { dg-error "expected identifier" } */
    abort ();

  MyRootClass.int;        /* { dg-error "expected identifier" } */
                          /* { dg-error "expected" "" { target *-*-* } 37 } */
  if (MyRootClass.int)    /* { dg-error "expected identifier" } */
                          /* { dg-error "expected" "" { target *-*-* } 39 } */
    abort ();

  return 0;
}
