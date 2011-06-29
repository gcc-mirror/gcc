#include <stdio.h>
#include <stdlib.h>
#include "../../../objc-obj-c++-shared/TestsuiteObject.m"

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
    @throw [TestsuiteObject new];
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

