/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is not equal to nil.  */
#include "../../objc-obj-c++-shared/runtime.h"
#include <objc/Protocol.h>

@protocol Foo
- (void)foo;
@end

int main (void)
{
  if (protocol_isEqual (@protocol(Foo), nil))
    {
      abort ();
    }
  
  return 0;
}

