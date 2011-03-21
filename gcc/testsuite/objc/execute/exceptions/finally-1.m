#include <stdio.h>
#include <stdlib.h>
//#import "../../../objc-obj-c++-shared/Object1.h"
#ifdef __OBJC2__
#include <objc/runtime.h>
@interface Object
+ initialize;
+ new;
- free;
@end
@implementation Object
+ initialize { return self; }
+ new  { return class_createInstance (self, 0); }
- free { return object_dispose(self);}
@end

#else
#import "../../../objc-obj-c++-shared/Object1.h"
#endif

static int made_try = 0;

int
thrower_try_body()
{
  made_try++;
  return (0);
}

static int made_finally = 0;

int
finally_body()
{
  made_finally++;
  return (0);
}

int
thrower()
{
  @try
  {
    thrower_try_body();
    @throw [Object new];
  }
  @finally
  {
    finally_body();
  }     
  return 0;
}

static int made_catch = 0;

int 
main(int ac, char *av[])
{
  @try
  {
    thrower();
  }
  @catch (id exc)
  {
    made_catch++;
    [exc free];
  }
  if (made_try != 1)
    abort ();
  if (made_finally != 1)
    abort ();
  if (made_catch != 1)
    abort ();
  return 0;
}
//#import "../../../objc-obj-c++-shared/Object1-implementation.h"
