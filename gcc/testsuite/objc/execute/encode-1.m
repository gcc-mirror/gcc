/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <stdlib.h>
#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>
#include <objc/objc-api.h>

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

  if (strcmp ("@", @encode (Object *)))
    {
      abort ();
    }

  if (strcmp (":", @encode (SEL)))
    {
      abort ();
    }

  return 0;
}
