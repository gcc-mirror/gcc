/*
 * Contributed by Nicola Pero <n.pero@mi.flashnet.it>
 * Tue Sep 19 4:34AM
 */
#include <objc/objc.h>
#include <objc/Protocol.h>

@protocol MyProtocol
+ (oneway void) methodA;
@end

@interface MyObject <MyProtocol>
@end

@implementation MyObject
+ (oneway void) methodA
{
  printf ("methodA\n");
}
@end

int main (void)
{
  [MyObject methodA];

   exit (0);
}


