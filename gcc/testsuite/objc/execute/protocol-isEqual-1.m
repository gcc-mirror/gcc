/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is equal to itself.  */
#include <objc/Protocol.h>
#include "../../objc-obj-c++-shared/runtime.h"

@protocol Foo
- (void)foo;
@end

int main (void)
{
  Protocol *protocol = @protocol(Foo);

  if (!protocol_isEqual (protocol, protocol))
    {
      abort ();
    }
  
  return 0;
}

