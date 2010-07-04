/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */

#include <stdlib.h>
#import "../../objc-obj-c++-shared/Object1.h"

/* Tests defining a protocol and a class adopting it */

@protocol Enabling
- (BOOL) isEnabled;
- (void) setEnabled: (BOOL)flag;
@end

@interface Feature : Object <Enabling>
{
  const char *name;
  BOOL isEnabled;
}
@end

@implementation Feature
- (BOOL) isEnabled
{
  return isEnabled;
}
- (void) setEnabled: (BOOL)flag
{
  isEnabled = flag;
}
@end

int main (void)
{
  Feature *object;

  object = [Feature new];

  [object setEnabled: YES];
  if (![object isEnabled])
    {
      abort ();
    }

  return 0;
}

