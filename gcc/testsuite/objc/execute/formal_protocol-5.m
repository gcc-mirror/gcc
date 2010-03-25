/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */

#include <stdlib.h>
#include "../../objc-obj-c++-shared/Protocol1.h"

/* Test defining a protocol, and accessing it using @protocol */

@protocol Evaluating
- (int) importance;
@end

/* A class adopting the protocol */
@interface Test <Evaluating>
@end

@implementation Test
- (int) importance
{
  return 1000;
}
@end

int main (void)
{
  Protocol *protocol = @protocol (Evaluating);

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  if (strcmp (protocol_getName(protocol), "Evaluating"))
#else
  if (strcmp ([protocol name], "Evaluating"))
#endif
    {
      abort ();
    }

  return 0;
}

