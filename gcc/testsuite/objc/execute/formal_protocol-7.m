/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */
#include <objc/objc.h>
#include <objc/Object.h>
#include <objc/Protocol.h>

/* Test defining two protocols, one incorporating the other one. */

@protocol Configuring
- (void) configure;
@end

@protocol Processing <Configuring>
- (void) process;
@end

/* A class adopting the protocol */
@interface Test : Object <Processing>
{
  BOOL didConfigure;
  BOOL didProcess;
}
@end

@implementation Test
- (void) configure
{
  didConfigure = YES;
}
- (void) process
{
  didProcess = YES;
}
@end

int main (void)
{
  id <Processing> object = [Test new];

  [object configure];
  [object process];

  return 0;
}

