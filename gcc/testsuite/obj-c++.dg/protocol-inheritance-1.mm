/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */
/* { dg-options "-Wno-protocol" } */

#include <objc/objc.h>

/* Test the -Wno-protocol flag.  With this, at a class is accepted
   (with no warnings) as conforming to a protocol even if some
   protocol methods are implemented in the superclass.  */

@protocol MyProtocol
- (int)method;
@end

@protocol MyProtocol2
- (int)method2;
@end

/* The superclass implements the method required by the protocol.  */
@interface MyRootClass
{
  Class isa;
}
- (int)method;
@end

@implementation MyRootClass
- (int)method
{
  return 23;
}
@end

/* The subclass inherits the method (does not implement it directly)
   but that still makes it conform to the protocol.  No warnings.  */
@interface MySubClass : MyRootClass <MyProtocol>
@end

@implementation MySubClass
@end /* No warnings here.  */


/* The subclass instead does not inherit the method method2 (and does
   not implement it directly) so it does not conform to the
   protocol MyProtocol2.  */
@interface MySubClass2 : MyRootClass <MyProtocol2>
@end

@implementation MySubClass2
@end /* Warnings here, below.  */
/* { dg-warning "incomplete implementation of class .MySubClass2." "" { target *-*-* } .-1 } */
/* { dg-warning "method definition for .\\-method2. not found" "" { target *-*-* } .-2 } */
/* { dg-warning "class .MySubClass2. does not fully implement the .MyProtocol2. protocol" "" { target *-*-* } .-3 } */
