/* Make sure that @synchronized parses and a very basic test runs.  */
/* { dg-options "-fobjc-exceptions -fgnu-runtime" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

int main (void)
{
  TestsuiteObject *a = [TestsuiteObject new];
  TestsuiteObject *b = [TestsuiteObject new];
  TestsuiteObject *c = [TestsuiteObject new];

  /* This single-threaded test just checks that @synchronized() uses a
     recursive mutex, and that the runtime at least doesn't crash
     immediately upon finding it.
  */
  @synchronized (a)
    {
      @synchronized (a)
	{
	  @synchronized (b)
	    {
	      @synchronized (b)
		{
		  @synchronized (c)
		    {
		      @synchronized (c)
			{
			  return 0;
			}
		    }
		}
	    }
	}
    }
}
