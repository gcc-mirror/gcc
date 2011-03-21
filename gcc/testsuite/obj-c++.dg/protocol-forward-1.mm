/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* Test that all protocols appearing in @interface declarations are
   real (ie, we saw a full @protocol definition with list of methods),
   and not just forward-references (ie, "@protocol NSObject;").  */

#include <objc/objc.h>

@protocol MyProtocol;

@protocol MyProtocol2
- (int)method2;
@end

@interface MyClass <MyProtocol> /* { dg-warning "definition of protocol .MyProtocol. not found" } */
@end

@interface MyClass2 <MyProtocol2> /* Ok */
@end

@interface MyClass2 (Category) <MyProtocol>  /* { dg-warning "definition of protocol .MyProtocol. not found" } */
@end

@protocol MyProtocol3 <MyProtocol> /* Ok */
@end

