/* Ensure that the compiler gracefully handles missing protocol declarations.
   In addition to not crashing :-), the compiler should properly handle
   valid protocol references, even when they're mixed with invalid ones.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>

@protocol DefinedProtocol
- (id) missingMethod1;
@end

@interface MyClass <UndefinedProtocol, DefinedProtocol>
/* { dg-error "cannot find protocol declaration for .UndefinedProtocol." "" { target *-*-* } .-1 } */
@end

@implementation MyClass
+(Class)class
{
  return self;
}
@end
/* { dg-warning "incomplete implementation of class .MyClass." "" { target *-*-* } .-1 } */
/* { dg-warning "method definition for .\\-missingMethod1. not found" "" { target *-*-* } .-2 } */
/* { dg-warning "class .MyClass. does not fully implement the .DefinedProtocol. protocol" "" { target *-*-* } .-3 } */

