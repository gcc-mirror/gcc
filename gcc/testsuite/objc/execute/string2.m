/* Based on a test case contributed by Nicola Pero.  */

#include <string.h>
#include <stdlib.h>
#include <objc/NXConstStr.h>

int main(int argc, void **args)
{
  if (strcmp ([@"this " @"is " @"a " @"string" cString],
              "this " "is " "a " "string"))
    abort ();
  return 0;
}
