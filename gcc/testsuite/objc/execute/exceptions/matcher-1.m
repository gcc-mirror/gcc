/* Test custom exception matchers  */
/* Author: Nicola Pero */

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

int
my_exception_matcher(Class match_class, id exception)
{
  /* Always matches.  */
  return 1;
}

@interface A : TestsuiteObject
@end

@implementation A
@end

@interface B : TestsuiteObject
@end

@implementation B
@end

int 
main(int argc, char *argv[])
{
  objc_setExceptionMatcher (my_exception_matcher);

  @try
    {
      @throw [A new];
    }
  @catch (B *exception)
    {
      /* Since we installed an exception matcher that always matches,
	 the exception should be sent here even if it's of class A and
	 this is looking for exceptions of class B.
       */
      return 0;
    }
  @catch (id exception)
    {
      abort ();
    }

  abort ();
}


#endif
