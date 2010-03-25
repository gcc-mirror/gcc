/* Crash due to descriptionFor(Instance|Class)Method applied to
   a protocol with no instance/class methods respectively.
   Problem report and original fix by richard@brainstorm.co.uk.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/Protocol1.h"
#include "../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>

@protocol NoInstanceMethods
+ testMethod;
@end

@protocol NoClassMethods
- testMethod;
@end

int
main()
{
[@protocol(NoInstanceMethods) descriptionForInstanceMethod: @selector(name)];
[@protocol(NoInstanceMethods) descriptionForClassMethod: @selector(name)];
[@protocol(NoClassMethods) descriptionForInstanceMethod: @selector(name)];
[@protocol(NoClassMethods) descriptionForClassMethod: @selector(name)];
return 0;
}
