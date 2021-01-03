/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test deprecate attribute with a forward declarations of
   @protocol.  */

#include <stdlib.h>
#include <objc/objc.h>

__attribute__ ((deprecated))
@protocol DeprecatedProtocol1;

@protocol NonDeprecatedProtocol1;

void function1 (id <DeprecatedProtocol1> object); /* { dg-warning "is deprecated" } */
void function2 (id <NonDeprecatedProtocol1> object);

@class Class4;

void function3 (Class4 <DeprecatedProtocol1> *object); /* { dg-warning "is deprecated" } */
void function4 (Class4 <NonDeprecatedProtocol1> *object);
void function5 (Class4 <NonDeprecatedProtocol1, DeprecatedProtocol1> *object); /* { dg-warning "is deprecated" } */

int function6 (void)
{
  Protocol *p1 = @protocol (DeprecatedProtocol1); /* { dg-warning "is deprecated" } */
  Protocol *p2 = @protocol (NonDeprecatedProtocol1);

  return (p1 == p2);
}

