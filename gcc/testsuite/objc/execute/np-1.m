/*
 * Contributed by Nicola Pero <n.pero@mi.flashnet.it>
 * Tue Sep 19 4:29AM
 */

#include <objc/objc.h>

@protocol MyProtocol
- (oneway void) methodA;
@end

@interface MyObject <MyProtocol>
@end

@implementation MyObject
- (oneway void) methodA
{
}
@end

int main (void)
{
  MyObject *object = nil;

  [object methodA];

  return 0;
}


