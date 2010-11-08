/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test case when an accessor from a @property matches a method
   required by a protocol.  If the @property is @dynamic, then no
   warning should be generated.  */

#include <objc/objc.h>
#include <objc/runtime.h>
#include <stdlib.h>

@protocol Count
- (int) count;
@end

@interface MyRootClass <Count>
{
  Class isa;
}
@property int count;
@end

@implementation MyRootClass
/* This @dynamic turns off any warnings for -count and -setCount:.  */
@dynamic count;
@end
