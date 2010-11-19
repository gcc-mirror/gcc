/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test deprecate attribute with a forward declarations of
   @protocol.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

__attribute__ ((deprecated))
@protocol DeprecatedProtocol1;

@protocol NonDeprecatedProtocol1;


@interface Class1 <DeprecatedProtocol1> /* { dg-warning "is deprecated" } */
@end

@interface Class2 <NonDeprecatedProtocol1>
@end

@interface Class3 <NonDeprecatedProtocol1, DeprecatedProtocol1> /* { dg-warning "is deprecated" } */
@end

@interface Class2 (Category1) <DeprecatedProtocol1> /* { dg-warning "is deprecated" } */
@end

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

