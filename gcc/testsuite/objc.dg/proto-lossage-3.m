/* Crash due to descriptionFor(Instance|Class)Method applied to
   a protocol with no instance/class methods respectively.
   Problem report and original fix by richard@brainstorm.co.uk.  */
/* { dg-do run } */
#include <objc/objc.h>
#include <objc/Object.h>
#include <objc/Protocol.h>

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
