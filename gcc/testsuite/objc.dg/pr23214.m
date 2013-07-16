/* Test that there is no problem initializing multiple static
   Protocol instances.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-framework Foundation" { target { { *-*-darwin* } && objc2 } } } */

#if defined (__NEXT_RUNTIME__) && defined(__OBJC2__) \
    && defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) \
    && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1070
#include <objc/Protocol.h>
#define OBJECT NSObject
#else
#include <objc/Object.h>
#define OBJECT Object
#include <objc/Protocol.h>
#endif

@interface OBJECT (TS_CAT)
- test;
@end

@implementation OBJECT (TS_CAT)
- test { return self; }
@end

@protocol A
@end

@protocol B 
@end

@interface Dummy : OBJECT <B>
@end

int main ()
{
  [@protocol(A) test];
  [@protocol(B) test];

  return 0;
}

@implementation Dummy
@end
