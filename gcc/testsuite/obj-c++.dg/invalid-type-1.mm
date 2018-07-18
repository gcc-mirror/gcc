/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
#include <objc/objc.h>

typedef int Integer;

@class MyClass;

typedef MyClass AClass;

@protocol MyProtocol
- (void) method;
@end

Class <MyProtocol> class_object; /* This is fine.  */

id <MyProtocol> object; /* This is fine.  */

AClass <MyProtocol> *object1; /* This is fine.  */

Integer <MyProtocol> *object2; /* { dg-error "'Integer' {aka 'int'} is not a template" } */
/* { dg-error ".MyProtocol. was not declared in this scope" "" { target *-*-* } .-1 } */

Integer <NonExistingProtocol> *object3; /* { dg-error "'Integer' {aka 'int'} is not a template" } */
/* { dg-error ".NonExistingProtocol. was not declared in this scope" "" { target *-*-* } .-1 } */
