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

Integer <MyProtocol> *object2; /* { dg-error "only Objective-C object types can be qualified with a protocol" } */

Integer <NonExistingProtocol> *object3; /* { dg-error "only Objective-C object types can be qualified with a protocol" } */
/* { dg-error "cannot find protocol" "" { target *-*-* } 23 } */
