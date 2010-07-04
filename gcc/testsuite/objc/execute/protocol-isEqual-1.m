/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is equal to itself.  */
#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol Foo
- (void)foo;
@end

int main (void)
{
  Protocol *protocol = @protocol(Foo);

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  if ( !protocol_isEqual (protocol, protocol))
#else
  if (! [protocol isEqual: protocol])
#endif
    {
      abort ();
    }
  
  return 0;
}

