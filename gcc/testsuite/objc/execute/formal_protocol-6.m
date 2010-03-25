/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */

#include <stdlib.h>
#include "../../objc-obj-c++-shared/Protocol1.h"

/* Test defining a protocol, and accessing it using @protocol */

@protocol Evaluating
- (int) importance;
@end

/* Without a class adopting the protocol - this doesn't work 
   with gcc-2.95.2 as well */

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

