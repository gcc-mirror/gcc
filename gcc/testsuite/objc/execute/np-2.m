/*
 * Contributed by Nicola Pero <n.pero@mi.flashnet.it>
 * Tue Sep 19 4:34AM
 */
#include "../../objc-obj-c++-shared/Protocol1.h"
#include <objc/objc.h>

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
#ifdef __NEXT_RUNTIME__                                   
+ initialize { return self; }
#endif
@end

int main (void)
{
  [MyObject methodA];

   exit (0);
}


