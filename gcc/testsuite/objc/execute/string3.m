/* Based on a test case contributed by Nicola Pero.  */

#include <string.h>
#include <stdlib.h>

#ifdef __NEXT_RUNTIME__
#include "next_mapping.h"
#else
#include <objc/NXConstStr.h>
#endif

#define STRING "this is a string"

int main (int argc, void **args)
{
  if (strcmp ([@STRING cString], STRING))
    abort ();
  return 0;
}
