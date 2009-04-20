/* Test custom exception handlers  */
/* Author: David Ayers */

#ifdef __NEXT_RUNTIME__
/* This test only runs for the GNU runtime. */

int main(void)
{
  return 0;
}

#else

#include <objc/objc-api.h>
#include <objc/Object.h>
#include <stdio.h>
#include <stdlib.h>

static unsigned int handlerExpected = 0;

void
my_exception_handler(id excp)
{
  /* Returning from the handler would abort.  */
  if (handlerExpected)
    exit(0);

  abort();
}

int 
main(int argc, char *argv[])
{
  _objc_unexpected_exception = my_exception_handler;

  @try
    {
      @throw [Object new];
    }
  @catch (id exc)
    {
      handlerExpected = 1;
    }

  @throw [Object new];
  abort();
  return 0;
}


#endif
