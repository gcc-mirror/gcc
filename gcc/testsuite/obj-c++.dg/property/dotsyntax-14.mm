/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test dot-syntax with accessors to be looked up in protocol @properties.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@protocol ProtocolA
@property int countA;
@end

@protocol ProtocolB
@property int countB;
@end

@protocol ProtocolC
@property int countC;
@end

@interface MyRootClass
{
  Class isa;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@interface MySubClass <ProtocolA, ProtocolB, ProtocolC>
@end

int function (MySubClass *object, int x)
{
  object.countA = x;
  object.countB = x;
  object.countC = object.countB;

  return object.countC;
}

int function2 (MyRootClass <ProtocolA, ProtocolB, ProtocolC> *object, int x)
{
  object.countA = x;
  object.countB = x;
  object.countC = object.countB;

  return object.countC;
}

int function3 (MyRootClass <ProtocolA, ProtocolB> *object, int x)
{
  object.countA = x;
  object.countB = x;
  object.countC = object.countB; /* { dg-error "request for member .countC. in" } */

  return object.countC;          /* { dg-error "request for member .countC. in" } */
}

int function4 (id <ProtocolA, ProtocolB, ProtocolC> object, int x)
{
  object.countA = x;
  object.countB = x;
  object.countC = object.countB;

  return object.countC;
}

int function5 (id <ProtocolA, ProtocolB> object, int x)
{
  object.countA = x;
  object.countB = x;
  object.countC = object.countB; /* { dg-error "request for member .countC. in" } */

  return object.countC;          /* { dg-error "request for member .countC. in" } */
}
