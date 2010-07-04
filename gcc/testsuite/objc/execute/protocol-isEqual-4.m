/* Contributed by David Ayers - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is not equal to something which is not a protocol.  */

#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol Foo
- (void)foo;
@end

int main (void)
{
  /* A Protocol object should not be equal to a Class object.  */
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  if (protocol_isEqual (@protocol(Foo), objc_getClass("Protocol")))
#else
  if ([@protocol(Foo) isEqual: [Protocol class]])
#endif
    {
      abort ();
    }
  
  return 0;
}

