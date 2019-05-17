/* Test for lookup of class (factory) methods.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface MyBase 
- (void) rootInstanceMethod;
@end

@interface MyIntermediate: MyBase
@end

@interface MyDerived: MyIntermediate
- (void) instanceMethod;
+ (void) classMethod;
@end

@implementation MyDerived
- (void) instanceMethod {
}

+ (void) classMethod {                    /* If a class method is not found, the root  */
    [self rootInstanceMethod];            /* class is searched for an instance method  */
    [MyIntermediate rootInstanceMethod];  /* with the same name.                       */

    [self instanceMethod];   /* { dg-warning ".MyDerived. may not respond to .\\+instanceMethod." } */
    /* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */
    [MyDerived instanceMethod];   /* { dg-warning ".MyDerived. may not respond to .\\+instanceMethod." } */
}
@end

