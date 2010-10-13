/* Test that there is no problem initializing multiple static
   Protocol instances.  */

/* { dg-do run } */

#include <objc/Object.h>
#include <objc/Protocol.h>

@protocol A
@end

@protocol B 
@end

@interface Dummy : Object <B>
@end

int main ()
{
  [@protocol(A) class];
  [@protocol(B) class];

  return 0;
}

@implementation Dummy
@end
