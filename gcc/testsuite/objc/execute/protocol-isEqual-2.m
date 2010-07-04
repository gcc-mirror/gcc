/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that protocols with different names are different.  */

#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol Foo1
- (void)foo1;
@end

@protocol Foo2
- (void)foo2;
@end

int main (void)
{
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  if (protocol_isEqual (@protocol(Foo1), @protocol(Foo2)))
#else
  if ([@protocol(Foo1) isEqual: @protocol(Foo2)])
#endif
    {
      abort ();
    }
  
  return 0;
}

