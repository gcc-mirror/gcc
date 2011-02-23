/* { dg-do compile } */
#include <objc/objc.h>

@interface Foo
{
  Class isa;
}
- (void) doSomething:(id object; /* { dg-error "xpected .\\)." } */
- (void) someOtherMethod;
@end
