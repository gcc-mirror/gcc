/* Crash due to descriptionFor(Instance|Class)Method applied to
   a protocol with no instance/class methods respectively.
   Problem report and original fix by richard@brainstorm.co.uk.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include <objc/objc.h>
#include "../objc-obj-c++-shared/runtime.h"

@interface MyClass
- name;
@end

@protocol NoInstanceMethods
+ testMethod;
@end

@protocol NoClassMethods
- testMethod;
@end

int
main()
{
protocol_getMethodDescription (@protocol(NoInstanceMethods), @selector(name), YES, YES);
protocol_getMethodDescription (@protocol(NoInstanceMethods), @selector(name), YES, NO);
protocol_getMethodDescription (@protocol(NoClassMethods), @selector(name), YES, YES);
protocol_getMethodDescription (@protocol(NoClassMethods), @selector(name), YES, NO);
return 0;
}
