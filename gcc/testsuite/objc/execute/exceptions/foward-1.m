/* Check that throwing an exception from a -forward:: works.  */
/* Developed by Marcin Koziej <creep@desk.pl>.  */

#import <objc/Object.h>
#import <objc/objc-api.h>
#include <stdlib.h>

static int i;

@interface Thrower : Object
- forward: (SEL) s : (void*) a;
@end

@implementation Thrower
- forward: (SEL) s : (void*) a
{
  i++;
  @throw [Object new];
}
@end
int
main()
{
  id t = [Thrower new];
  @try
  {
    [t doesnotexist];
  }
  @catch (id error)
  {
    i++;
    [error free];
  }
  
  if (i != 2)
    abort ();
  
  return 0;
}
