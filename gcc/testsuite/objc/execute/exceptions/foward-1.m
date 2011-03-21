/* Check that throwing an exception from a -forward:: works.  */
/* Developed by Marcin Koziej <creep@desk.pl>.  */

#include <stdlib.h>
#include <objc/Object.h>
#ifndef __NEXT_RUNTIME__
#import <objc/objc-api.h>
#endif

#ifdef __OBJC2__
@interface Object (TEST_SUITE_ADDITIONS)
+ initialize;
+ alloc;
+ new;
- init;
- free;
@end

@implementation Object (TEST_SUITE_ADDITIONS)
+ initialize { return self; }
+ alloc { return class_createInstance (self, 0); }
+ new { return [[self alloc] init]; }
- init { return self; }
- free { return object_dispose(self); }
@end
#endif

static int i;

__attribute__((objc_exception)) 
@interface Thrower : Object 
- forward: (SEL) s : (void*) a;
@end

@implementation Thrower
- forward: (SEL) s : (void*) a
{
  i++;
  @throw [Object new];
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
