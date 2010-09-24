/* Make sure that @synchronized parses and a very basic test runs.  */
/* { dg-options "-fobjc-exceptions -fgnu-runtime" } */

#include "../objc-obj-c++-shared/Object1.h"

int main (void)
{
  Object *a = [Object new];
  Object *b = [Object new];
  Object *c = [Object new];

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
