/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* This test tests class extensions and protocols.  */

#include <objc/objc.h>

/* First, a simple test where a plain class has a protocol attached to
   it in a class extension.  */
@interface MyObject
{
  Class isa;
}
@end

@protocol MyProtocol
- (void) test;
@end

@interface MyObject () <MyProtocol>
@end

@implementation MyObject
@end /* { dg-warning "incomplete implementation of class .MyObject." } */
     /* { dg-warning "method definition for .-test. not found" "" { target *-*-* } .-1 } */
     /* { dg-warning "class .MyObject. does not fully implement the .MyProtocol. protocol" "" { target *-*-* } .-2 } */



/* Second, a more interesting test where protocols are added from the
   main class and from two different class extensions.  */
@interface MyObject2 : MyObject <MyProtocol>
@end

@protocol MyProtocol2
- (void) test2;
@end

@protocol MyProtocol3
- (void) test3;
@end

@interface MyObject2 () <MyProtocol2>
@end

@interface MyObject2 () <MyProtocol3>
@end

@implementation MyObject2
@end /* { dg-warning "incomplete implementation of class .MyObject2." } */
     /* { dg-warning "method definition for .-test. not found" "" { target *-*-* } .-1 } */
     /* { dg-warning "class .MyObject2. does not fully implement the .MyProtocol. protocol" "" { target *-*-* } .-2 } */
     /* { dg-warning "method definition for .-test2. not found" "" { target *-*-* } .-3 } */
     /* { dg-warning "class .MyObject2. does not fully implement the .MyProtocol2. protocol" "" { target *-*-* } .-4 } */
     /* { dg-warning "method definition for .-test3. not found" "" { target *-*-* } .-5 } */
     /* { dg-warning "class .MyObject2. does not fully implement the .MyProtocol3. protocol" "" { target *-*-* } .-6 } */
