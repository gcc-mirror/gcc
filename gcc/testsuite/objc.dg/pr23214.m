/* Test that there is no problem initializing multiple static
   Protocol instances.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <objc/Protocol.h>
 
@interface Object (TS_CAT)
- test;
@end

@implementation Object (TS_CAT)
- test { return self; }
@end

@protocol A
@end

@protocol B 
@end

@interface Dummy : Object <B>
@end

int main ()
{
  [@protocol(A) test];
  [@protocol(B) test];

  return 0;
}

@implementation Dummy
@end
