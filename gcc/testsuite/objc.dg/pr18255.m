/* This is a test for a GNU Objective-C Runtime library bug.  */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

#include <objc/Protocol.h>
#include <stdlib.h>

@protocol a
- aMethod;
@end


@protocol b <a>
- bMethod;
@end


int main (int argc, char **argv)
{
  if ([@protocol(b) descriptionForInstanceMethod: @selector(aMethod)] == NULL)
    abort ();

  return 0;
}
