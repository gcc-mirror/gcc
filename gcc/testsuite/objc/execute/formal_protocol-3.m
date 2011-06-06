/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */

#include <stdlib.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.m"

/* Test defining two protocol, a class adopting both of them, 
   and using an object of type `id <Protocol1, Protocol2>' */ 

@protocol Enabling
- (BOOL) isEnabled;
- (void) setEnabled: (BOOL)flag;
@end

@protocol Evaluating
- (int) importance;
@end

@interface Feature : TestsuiteObject <Enabling, Evaluating>
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
- (int) importance
{
  return 1000;
}
@end

int main (void)
{
  id <Enabling, Evaluating> object;

  object = [Feature new];

  [object setEnabled: YES];
  if (![object isEnabled])
    {
      abort ();
    }

  if ([object importance] != 1000)
    {
      abort ();
    }

  return 0;
}

