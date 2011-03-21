/* Test that there is no problem initializing multiple static
   Protocol instances.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <objc/Protocol.h>
 
#ifdef __OBJC2__
/* The ObjC V2 "Object" does not provide -class.  */
@interface Object (TS_CAT)
- class;
@end

@implementation Object (TS_CAT)
- class { return isa; }
@end
#endif

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
