/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <stdlib.h>
#import "../../objc-obj-c++-shared/TestsuiteObject.h"
#include <objc/objc.h>

/* Test very simple @encode */

int main (void)
{
  if (strcmp ("i", @encode (int)))
    {
      abort ();
    }

  if (strcmp ("@", @encode (id)))
    {
      abort ();
    }

  if (strcmp ("@", @encode (TestsuiteObject *)))
    {
      abort ();
    }

  if (strcmp (":", @encode (SEL)))
    {
      abort ();
    }

  return 0;
}
