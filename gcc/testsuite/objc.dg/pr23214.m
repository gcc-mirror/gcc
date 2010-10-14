/* Test that there is no problem initializing multiple static
   Protocol instances.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../objc-obj-c++-shared/Object1.m" } */

#import "../objc-obj-c++-shared/Object1.h"
#import "../objc-obj-c++-shared/Protocol1.h"

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
