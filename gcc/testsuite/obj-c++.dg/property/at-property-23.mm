/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that properties of type arrays or bitfields are rejected.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
}
@property int a[8]; /* { dg-error "property cannot be an array" } */
@property int b:8;  /* { dg-error "expected" } */
@property int c[];  /* { dg-error "property cannot be an array" } */
/* { dg-error "forbids flexible array member" "flexible array member" { target *-*-* } .-1 } */
@end
