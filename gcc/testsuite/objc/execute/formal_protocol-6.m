/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */
#include <objc/objc.h>
#include <objc/Protocol.h>

/* Test defining a protocol, and accessing it using @protocol */

@protocol Evaluating
- (int) importance;
@end

/* Without a class adopting the protocol - this doesn't work 
   with gcc-2.95.2 as well */

int main (void)
{
  Protocol *protocol = @protocol (Evaluating);

  if (strcmp ([protocol name], "Evaluating"))
    {
      abort ();
    }

  return 0;
}

