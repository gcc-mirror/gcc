/* Check that throwing an exception from a -forward:: works.  */
/* Developed by Marcin Koziej <creep@desk.pl>.  */

#include <stdlib.h>
#import "../../../objc-obj-c++-shared/Object1.h"
#import <objc/objc-api.h>

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
