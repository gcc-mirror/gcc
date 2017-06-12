/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
@end

@implementation MyRootClass
@end

/* Test @property/@dynamic with protocols.  */

@protocol MyProtocol
@property int a;
@end


/* This class is declared to conform to the protocol, but because of
   @dynamic, no warnings are issued even if the getter/setter for the
   @property are missing.  */
@interface MyClass1 : MyRootClass <MyProtocol>
@end

@implementation MyClass1
@dynamic a;
@end


/* This class is declared to conform to the protocol and warnings are
   issued because the setter for the @property is missing.  */
@interface MyClass2 : MyRootClass <MyProtocol>
@end

@implementation MyClass2
- (int) a
{
  return 0;
}
@end /* { dg-warning "incomplete implementation" } */
/* { dg-warning "method definition for .-setA:. not found" "" { target *-*-* } .-1 } */
/* { dg-warning "class .MyClass2. does not fully implement the .MyProtocol. protocol" "" { target *-*-* } .-2 } */
