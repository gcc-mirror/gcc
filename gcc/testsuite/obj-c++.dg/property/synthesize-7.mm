/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* Test @synthesize with protocols of protocols.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@protocol ProtocolA
@property int countA;
@end

@protocol ProtocolB <ProtocolA>
@property int countB;
@end

@protocol ProtocolC <ProtocolB>
@property int countC;
@end

@protocol ProtocolD
@property int countD;
@end

@interface MyRootClass <ProtocolC>
{
  Class isa;
  int countA;
  int countB;
  int countC;
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize countA;
@synthesize countB;
@synthesize countC;
@end

@interface MySubClass : MyRootClass <ProtocolD>
{
  int countD;
}
@end

@implementation MySubClass
@synthesize countD;
@end

int main (void)
{
  MySubClass *object = [[MySubClass alloc] init];
  int i;

  for (i = 0; i < 10; i++)
    {
      object.countA += i;
      object.countB += i + 1;
      object.countC += i + 2;
      object.countD += i + 3;
    }

  if (object.countA != 45)
    abort ();

  if (object.countB != 55)
    abort ();

  if (object.countC != 65)
    abort ();

  if (object.countD != 75)
    abort ();

  return 0;
}


