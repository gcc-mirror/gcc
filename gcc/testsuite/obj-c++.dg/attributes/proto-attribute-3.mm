/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test deprecate attribute with normal @protocol declarations.  */


#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

__attribute__ ((deprecated))
@protocol DeprecatedProtocol1
- (void) aMethod;
@end

@protocol NonDeprecatedProtocol1
- (void) anotherMethod;
@end

@protocol Protocol2 <DeprecatedProtocol1> /* { dg-warning "is deprecated" } */
- (void) someOtherMethod;
@end

@protocol Protocol3 <NonDeprecatedProtocol1>
- (void) someOtherMethod2;
@end

@protocol Protocol4 <NonDeprecatedProtocol1, DeprecatedProtocol1> /* { dg-warning "is deprecated" } */
- (void) someOtherMethod3;
@end


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
