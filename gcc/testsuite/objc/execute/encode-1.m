/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>

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
