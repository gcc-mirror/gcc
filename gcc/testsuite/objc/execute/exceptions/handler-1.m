/* Test custom exception handlers  */
/* Author: David Ayers */

#ifdef __NEXT_RUNTIME__
/* This test only runs for the GNU runtime.  TODO: It should work on
   the NEXT runtime as well (needs testing).
 */

int main(void)
{
  return 0;
}

#else

#include <objc/runtime.h>
#include <objc/objc-exception.h>
#include "../../../objc-obj-c++-shared/TestsuiteObject.m"
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
  objc_setUncaughtExceptionHandler (my_exception_handler);

  @try
    {
      @throw [TestsuiteObject new];
    }
  @catch (id exc)
    {
      handlerExpected = 1;
    }

  @throw [TestsuiteObject new];
  abort();
  return 0;
}


#endif
