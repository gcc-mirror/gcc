/* Based on a test case contributed by Nicola Pero.  */

#include <string.h>
#include <stdlib.h>
#include <objc/NXConstStr.h>

#define STRING "this is a string"

int main (int argc, void **args)
{
  if (strcmp ([@STRING cString], STRING))
    abort ();
  return 0;
}
