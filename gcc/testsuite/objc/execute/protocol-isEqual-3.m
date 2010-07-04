/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is not equal to nil.  */

#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol Foo
- (void)foo;
@end

int main (void)
{
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  if (protocol_isEqual (@protocol(Foo), nil))
#else
  if ([@protocol(Foo) isEqual: nil])
#endif
    {
      abort ();
    }
  
  return 0;
}

