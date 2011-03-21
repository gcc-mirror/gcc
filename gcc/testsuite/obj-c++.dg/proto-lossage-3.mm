/* Crash due to descriptionFor(Instance|Class)Method applied to
   a protocol with no instance/class methods respectively.
   Problem report and original fix by richard@brainstorm.co.uk.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../objc-obj-c++-shared/Object1.mm" } */

#include "../objc-obj-c++-shared/next-mapping.h"
#include "../objc-obj-c++-shared/Protocol1.h"

@protocol NoInstanceMethods
+ testMethod;
@end

@protocol NoClassMethods
- testMethod;
@end

int
main()
{
#ifdef __OBJC2__
protocol_getMethodDescription(@protocol(NoInstanceMethods), @selector(name), NO, YES);
protocol_getMethodDescription(@protocol(NoInstanceMethods), @selector(name), NO, NO);
protocol_getMethodDescription(@protocol(NoClassMethods), @selector(name), NO, YES);
protocol_getMethodDescription(@protocol(NoClassMethods), @selector(name), NO, NO);
#else
[@protocol(NoInstanceMethods) descriptionForInstanceMethod: @selector(name)];
[@protocol(NoInstanceMethods) descriptionForClassMethod: @selector(name)];
[@protocol(NoClassMethods) descriptionForInstanceMethod: @selector(name)];
[@protocol(NoClassMethods) descriptionForClassMethod: @selector(name)];
#endif
return 0;
}
