/* Based on a test case contributed by Nicola Pero.  */

#import "../../objc-obj-c++-shared/next-mapping.h"
#include <string.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

#define STRING "this is a string"

int main (int argc, void **args)
{
  if (strcmp ([@STRING cString], STRING))
    abort ();
  return 0;
}
