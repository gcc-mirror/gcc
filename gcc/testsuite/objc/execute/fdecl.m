/* Bug report submitted by igorkh@hotbot.com on submit@bugs.debian.org
   Thu, 13 Apr 2000 09:42:08 -0400 */

@class AClass;

@interface test
{
        AClass *foo;
}
@end

@implementation test
@end

int main (void)
{
  return 0;
}

