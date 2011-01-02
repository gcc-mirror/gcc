/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

/* Test that all protocols appearing in @interface declarations are
   real (ie, we saw a full @protocol definition with list of methods),
   and not just forward-references (ie, "@protocol NSObject;").  This
   test checks protocols implemented by other protocols.  */

#include <objc/objc.h>

@protocol MyProtocol;

@interface MyClass <MyProtocol> /* { dg-warning "definition of protocol .MyProtocol. not found" } */
@end


@protocol MyProtocol2 <MyProtocol>
- (int)method2;
@end

@interface MyClass2 <MyProtocol2> /* { dg-warning "definition of protocol .MyProtocol. not found" } */
- (int)method2;
@end


@protocol MyProtocol3 <MyProtocol2>
- (int)method3;
@end

@interface MyClass3 <MyProtocol3>  /* { dg-warning "definition of protocol .MyProtocol. not found" } */
- (int)method2;
- (int)method3;
@end


@protocol MyProtocol4 <MyProtocol3, MyProtocol2>
- (int)method4;
@end

@interface MyClass4 <MyProtocol4>  /* { dg-warning "definition of protocol .MyProtocol. not found" } */
- (int)method2;
- (int)method3;
- (int)method4;
@end


@protocol MyProtocol5
- (int)method5;
@end

@interface MyClass5 <MyProtocol5> /* Ok */
- (int)method5;
@end


@protocol MyProtocol6 <MyProtocol5>
- (int)method6;
@end

@interface MyClass6 <MyProtocol6> /* Ok */
- (int)method5;
- (int)method6;
@end


@protocol MyProtocol7 <MyProtocol5, MyProtocol4>
- (int)method7;
@end

@interface MyClass7 <MyProtocol7> /* { dg-warning "definition of protocol .MyProtocol. not found" } */
- (int)method2;
- (int)method3;
- (int)method4;
- (int)method5;
- (int)method7;
@end


/* Now test that if we finally define MyProtocol, the warnings go away.  */
@protocol MyProtocol
- (int)method;
@end

@protocol MyProtocol8 <MyProtocol5, MyProtocol4>
- (int)method8;
@end

@interface MyClass8 <MyProtocol8> /* Ok */
- (int)method;
- (int)method2;
- (int)method3;
- (int)method4;
- (int)method5;
- (int)method8;
@end
