/* Contributed by Nicola Pero - Fri Mar  9 21:35:47 CET 2001 */
#include <objc/objc.h>
#include <objc/Protocol.h>

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

  if (strcmp ([protocol name], "Evaluating"))
    {
      abort ();
    }

  return 0;
}

