/* Contributed by Nicola Pero - Fri Jun  4 03:16:17 BST 2004 */
/* Test that a protocol is equal to itself.  */
#include <objc/Protocol.h>

@protocol Foo
- (void)foo;
@end

int main (void)
{
  Protocol *protocol = @protocol(Foo);

  if (! [protocol isEqual: protocol])
    {
      abort ();
    }
  
  return 0;
}

