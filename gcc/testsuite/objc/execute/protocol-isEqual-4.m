/* Contributed by David Ayers - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is not equal to something which is not a protocol.  */
#include "../../objc-obj-c++-shared/runtime.h"
#include <objc/Protocol.h>

@protocol Foo
- (void)foo;
@end

int main (void)
{
  /* A Protocol object should not be equal to a Class object.  */
  if (protocol_isEqual (@protocol(Foo), (id)objc_getClass("Protocol")))
    {
      abort ();
    }
  
  return 0;
}

