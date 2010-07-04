/* Based on a test case contributed by Nicola Pero.  */

#import "../../objc-obj-c++-shared/next-mapping.h"
#include <string.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

int main(int argc, void **args)
{
  if (strcmp ([@"this is a string" cString], "this is a string"))
    abort ();
  return 0;
}
