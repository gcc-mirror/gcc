/* Check that throwing an exception from a -forward:: works.  */
/* Developed by Marcin Koziej <creep@desk.pl>.  */

#include <stdio.h>
#include <stdlib.h>
#include "../../../objc-obj-c++-shared/TestsuiteObject.m"

static int i;

__attribute__((objc_exception)) 
@interface Thrower : TestsuiteObject 
- forward: (SEL) s : (void*) a;
@end

@implementation Thrower
- forward: (SEL) s : (void*) a
{
  i++;
  @throw [TestsuiteObject new];
  return nil;
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
